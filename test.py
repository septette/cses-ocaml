import http.cookiejar
import requests
import configparser
from bs4 import BeautifulSoup
import os
import shutil
from abc import *
import glob
import re
from dataclasses import *
from enum import *
import subprocess
import filecmp

def login(username, password):
    session = requests.Session()
    jar = http.cookiejar.CookieJar()
    session.cookies = jar
    
    url = "https://cses.fi/login"
    response = session.get(url)
    soup = BeautifulSoup(response.content, 'html.parser')
    values = {
        "nick": username,
        "pass": password,
        'csrf_token': soup.find('input', {'name': 'csrf_token'}).get('value')
    }

    response = session.post(url, data=values)

    if response.status_code == 200 and 'Wrong CSRF token' not in response.text:
        print(f"Logged in.")
        return session
    else:
        raise Exception(f"Login unsuccessful with status code {response.status_code}, data {response.content}")
    
def parse_config(filename):
    config = configparser.ConfigParser()
    config.read("config.ini")
    return config

class Problem:
    def __init__(self, code, session):
        self.code = code
        self.session = session
    
    def get_result_url(self):
        return f"https://cses.fi/problemset/view/{self.code}"
    
    def get_io_url(self):
        response = self.session.get(self.get_result_url())
        soup = BeautifulSoup(response.content, 'html.parser')
        a_elements = soup.find_all('a', class_='details-link')
        if not a_elements:
            raise NotImplementedError('Have not implemented submitting automatically yet. Make a submission to unlock test cases')
        return "https://cses.fi" + next(iter(a_elements)).get('href')
    
    def save(self, url, filepath, logging=False):
        response = requests.get(url)
        with open(filepath, 'w') as f:
            f.write(response.text)
        if logging:
            print(f'Saved {url} to {filepath}.')

    def clear_folder(self, directory):
        for root, dirs, files in os.walk(directory):
            for f in files:
                os.unlink(os.path.join(root, f))
            for d in dirs:
                shutil.rmtree(os.path.join(root, d))

    def get_problem_directory(self, root_directory):
        return os.path.join(root_directory, self.code)

    def download_io(self, root_directory):
        problem_directory = self.get_problem_directory(root_directory)
        if os.path.exists(problem_directory):
            _, _, files = next(os.walk(problem_directory))
            should_delete = input(f'Directory {problem_directory} exists with {len(files)} files inside. Delete files? (y/N) ')
            if should_delete.strip().lower() in ('y', 'yes'):
                self.clear_folder(problem_directory)
                print('Cleared files in directory.')
            else:
                print('Preserving existing files, exiting.')
                return
        else:
            os.mkdir(problem_directory)
        
        response = self.session.get(self.get_io_url())
        soup = BeautifulSoup(response.content, 'html.parser')
        a_elements = soup.find_all('a', class_='save')
        for i, a in enumerate(a_elements):
            if i % 3 == 2: continue
            if i % 3 == 0:
                # "input"
                self.save("https://cses.fi" + a.get('href'),
                          os.path.join(problem_directory, f"input_{i // 3 + 1}.txt"),
                          True)
            elif i % 3 == 1:
                # "correct output"
                self.save("https://cses.fi" + a.get('href'),
                          os.path.join(problem_directory, f"expected_{i // 3 + 1}.txt"),
                          True)
    
    def get_io_pairs(self, root_directory):
        problem_directory = self.get_problem_directory(root_directory)
        input_files = glob.glob(os.path.join(problem_directory, 'input_*.txt'))
        expected_files = set(glob.glob(os.path.join(problem_directory, 'expected_*.txt')))
        # (i, input, output) tuples (so that they end up sorted)
        results = []
        for infile in input_files:
            match = re.search(r'^.*/input_(.+)\.txt$', infile)
            if not match: continue
            i = match.group(1)
            expected_matches = [e for e in expected_files if re.search(f"^.*/expected_{i}.txt", e)]
            if len(expected_matches) != 1:
                print('Found too many expected matches, ignoring:', expected_matches)
            results.append((i, infile, expected_matches[0]))

        return sorted(results)

class Status(Enum):
    SUCCESS = 0
    FAIL = 1

class TerminalColors(Enum):
    CEND = '\33[0m'
    CGREENBG = '\33[42m'
    CREDBG = '\33[41m'

@dataclass
class Result:
    i: int
    result: Status = Status.FAIL

    def show(self):
        if self.result == Status.SUCCESS:
            return (TerminalColors.CGREENBG + " [ ✔ ] " +  TerminalColors.CEND + f" Test {self.i}")
        elif self.result == Status.FAIL:
            return TerminalColors.CREDBG + " [ ✘ ] " + TerminalColors.CEND + f"Test {self.i}"

class Runner(ABC):
    def __init__(self) -> None:
        super().__init__()
        self.results = []
        
    def reset_stats(self):
        self.results = []

    def compile(self, runfile):
        self.reset_stats()
        self.compile_lang(runfile)

    @abstractmethod
    def compile_lang(self, runfile):
        raise NotImplementedError("")

    @abstractmethod
    def get_default_runfile_name(self, code):
        raise NotImplementedError("")
    
    @abstractmethod
    def run(self, i, input_file, expected_file, runfile):
        raise NotImplementedError("")
    
    def stats(self):
        for stat in self.results:
            print(stat.show())
    
class OcamlRunner(Runner):
    def get_default_runfile_name(self, code):
        return code + '.ml'

    def compile_lang(self, runfile):
        subprocess.run("ocamlopt -o under_test " + runfile, shell=True)

    def run(self, i, input_file, expected_file, runfile):
        subprocess.run(f"./under_test < {input_file} > temp_file.txt")
        # todo - timing etc.
        status = Status.SUCCESS if filecmp.cmp('temp_file.txt', expected_file) else Status.FAIL
        self.stats.append(Result(i, status))

class RunnerFactory:
    @staticmethod
    def create(lang):
        if lang == 'ocaml':
            return OcamlRunner()

def run():
    config = parse_config('config.ini')
    username = config.get('DEFAULT', 'username')
    password = config.get('DEFAULT', 'password')
    directory = config.get('DEFAULT', 'directory')
    lang = config.get('DEFAULT', 'lang')
    session = login(username, password)

    runner = RunnerFactory.create(lang)

    code = input("Problem code: ").strip()
    problem = Problem(code, session)
    problem.download_io(directory)

    tentative_filepath = os.path.join(directory, runner.get_default_runfile_name(code))
    runfile = input(f"Submit file ({tentative_filepath}): ").strip() or tentative_filepath
    runner.compile(runfile)

    for i, input_file, expected_file in problem.get_io_pairs(directory):
        runner.run(i, input_file, expected_file)
    runner.stats()
run()
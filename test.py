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
import timeit

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
    config.read(filename)
    return config

def save_config(config, filename):
    with open(filename, 'w') as cf:
        config.write(cf)

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
    
    def get_a_for_title(self, soup, title):
        # Find all "input" elements headers. Then get the next siblings.
        tr_input = []
        for tr in soup.find_all('tr'):
            th = tr.find('th')
            if th and title in th.get_text():
                tr_input.append(tr.find_next_sibling().find('a', class_='save'))
        return tr_input

    def download_io(self, root_directory):
        problem_directory = self.get_problem_directory(root_directory)
        if os.path.exists(problem_directory):
            _, _, files = next(os.walk(problem_directory))
            should_delete = input(f'Directory {problem_directory} exists with {len(files)} files inside. Delete files? (y/N) ')
            if should_delete.strip().lower() in ('y', 'yes'):
                self.clear_folder(problem_directory)
                print('Cleared files in directory.')
            else:
                print('Using existing files.')
                return
        else:
            os.mkdir(problem_directory)
        
        response = self.session.get(self.get_io_url())
        soup = BeautifulSoup(response.content, 'html.parser')

        for i, a in enumerate(self.get_a_for_title(soup, 'input')):
            self.save("https://cses.fi" + a.get('href'),
                        os.path.join(problem_directory, f"input_{i + 1}.txt"),
                        True)
        for i, a in enumerate(self.get_a_for_title(soup, 'correct output')):
            self.save("https://cses.fi" + a.get('href'),
                        os.path.join(problem_directory, f"expected_{i + 1}.txt"),
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

        return sorted(results, key=lambda l: int(l[0]))

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
    time: float = 0

    # TODO - refactor so results are not responsible for display/formatting
    def show(self):
        if self.result == Status.SUCCESS:
            return TerminalColors.CGREENBG.value + " [ ✔ ] " +  TerminalColors.CEND.value + f" Test {self.i} | {str(round(self.time, 3))}s"
        elif self.result == Status.FAIL:
            return TerminalColors.CREDBG.value + " [ ✘ ] " + TerminalColors.CEND.value + f"Test {self.i} | {str(round(self.time, 3))}s"

class Runner(ABC):
    def __init__(self) -> None:
        super().__init__()
        self.results = []
        # TODO - refactor, this is unnecessary state
        self.compiled_location = None
        self.compiled_filename = None
        
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
    
    def run(self, i, input_file, expected_file):
        start = timeit.default_timer()
        output_file = self._run(i, input_file, expected_file)
        end = timeit.default_timer()
        
        status = Status.SUCCESS if self.compare_ignoring_whitespace(output_file, expected_file) else Status.FAIL
        result = Result(i, status, end - start)
        print(result.show())
        self.results.append(result)
    
    @abstractmethod
    # should return the location of the output file
    def _run(self, i, input_file, expected_file, runfile) -> str:
        raise NotImplementedError("")
    
    def stats(self):
        print(f"{sum([1 if r.result == Status.SUCCESS else 0 for r in self.results])} / {len(self.results)} tests passed!")

    def compare_ignoring_whitespace(self, file_1, file_2):
        result = subprocess.run(f"diff -w {file_1} {file_2}", shell=True, capture_output=True, text=True)
        return result.stdout.strip() == ""
    
class OcamlRunner(Runner):
    def get_default_runfile_name(self, code):
        return code + '.ml'

    def compile_lang(self, runfile):
        subprocess.run("eval $(opam env)", shell=True)
        subprocess.run("ocamlopt -o under_test " + runfile, shell=True)
        self.compiled_filename = 'under_test'

    def _run(self, i, input_file, expected_file):
        subprocess.run(f"./{self.compiled_filename} < {input_file} > temp_file.txt", shell=True)
        return 'temp_file.txt'

class OcamlDuneRunner(Runner):
    def get_default_runfile_name(self, code):
        return f"cses_{code}.exe"

    def compile_lang(self, runfile):
        subprocess.run("eval $(opam env)", shell=True)
        (dirname, filename) = os.path.split(runfile)
        subprocess.run(f"cd {dirname} && dune build " + filename, shell=True)
        self.compiled_location = dirname
        self.compiled_filename = filename

    def _run(self, i, input_file, expected_file):
        # TODO - refactor, this is not very robust.
        subprocess.run(f"cd {self.compiled_location} && dune exec -- ./{self.compiled_filename} < {input_file} > temp_file.txt", shell=True)
        return os.path.join(self.compiled_location, 'temp_file.txt')

class RunnerFactory:
    @staticmethod
    def create(lang):
        if lang == 'ocaml':
            return OcamlRunner()
        elif lang == 'ocaml-dune':
            return OcamlDuneRunner()

def run():
    config = parse_config('config.ini')
    username = config.get('DEFAULT', 'username')
    password = config.get('DEFAULT', 'password')
    directory = config.get('DEFAULT', 'save_directory')
    lang = config.get('DEFAULT', 'lang')
    default_code = config.get('DEFAULT', 'code') if config.has_option('DEFAULT', 'code') else 'no_default'
    code_directory = config.get('DEFAULT', 'code_directory') if config.has_option('DEFAULT', 'code_directory') else os.getcwd()

    session = login(username, password)
    runner = RunnerFactory.create(lang)

    code = input(f"Problem code ({default_code}): ").strip()
    if not code: code = default_code
    # Save the code for next time to avoid typing
    config.set('DEFAULT', 'code', code)
    save_config(config, 'config.ini')

    problem = Problem(code, session)
    problem.download_io(directory)

    tentative_filepath = os.path.join(code_directory, runner.get_default_runfile_name(code))
    runfile = input(f"Submit file ({tentative_filepath}): ").strip() or tentative_filepath
    runner.compile(runfile)

    for i, input_file, expected_file in problem.get_io_pairs(directory):
        runner.run(i, input_file, expected_file)
    runner.stats()
run()
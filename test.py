import http.cookiejar
import requests
import configparser
from bs4 import BeautifulSoup
import os
import shutil

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

    def download_io(self, root_directory):
        problem_directory = os.path.join(root_directory, self.code)
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

def run():
    config = parse_config('config.ini')
    username = config.get('DEFAULT', 'username')
    password = config.get('DEFAULT', 'password')
    directory = config.get('DEFAULT', 'directory')
    lang = config.get('DEFAULT', 'lang')
    session = login(username, password)

    code = input("Problem code: ").strip()
    problem = Problem(code, session)
    problem.download_io(directory)

run()
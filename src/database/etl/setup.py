# https://the-hitchhikers-guide-to-packaging.readthedocs.io/en/latest/quickstart.html
from setuptools import find_packages, setup

setup(
    name="etl",
    version="0.1dev",
    packages=find_packages(),
    license="AGPL",
    long_description=open("README.md").read(),
    install_requires=open("requirements.txt").read().splitlines(),
)

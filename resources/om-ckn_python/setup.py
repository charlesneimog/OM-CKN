# python setup.py sdist bdist_wheel
from setuptools import setup, find_packages
import codecs
import os

here = os.path.abspath(os.path.dirname(__file__))

with codecs.open(os.path.join(here, "README.md"), encoding="utf-8") as fh:
    long_description = "\n" + fh.read()

VERSION = '0.0.0.6'
DESCRIPTION = 'This is a simple package to help musicians that do not use a lot of code. '
LONG_DESCRIPTION = 'CHARLESNEIMOG.COM'

# Setting up
setup(
    name="om_ckn",
    version=VERSION,
    author="Charles K. Neimog",
    author_email="<charlesneimog@outlook.com>",
    description=DESCRIPTION,
    long_description_content_type="text/markdown",
    long_description=long_description,
    packages=find_packages(),
    install_requires=['librosa', 'pedalboard', 'dawdreamer', 'numpy'],
    keywords=['python', 'music', 'OpenMusic'],
    classifiers=[
        "Operating System :: Unix",
        "Operating System :: MacOS :: MacOS X",
        "Operating System :: Microsoft :: Windows",
    ]
)

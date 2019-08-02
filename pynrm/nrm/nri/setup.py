"""Node Resource Interface """

from setuptools import setup, find_packages

setup(
    name='nri',
    version='0.0.1',
    description="Node Resource Interface",
    author='Argonne National Laboratory',
    author_email='vreis@anl.gov',
    url='http://argo-osr.org',
    license='BSD3',

    classifiers=[
        'Development Status :: 3 - Alpha',
        'License :: OSI Approved :: BSD License',
        'Programming Language :: Python :: 3.7',
    ],

    packages=find_packages(),
)

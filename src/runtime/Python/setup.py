from distutils.core import setup
import py2exe

setup(
    console=["pyhi"],
    options = {"py2exe": {"compressed": 1,
                          "optimize": 0, #IMPORTANT, higher levels break Pyhi
                          "ascii": 1,
                          "bundle_files": 1}},
    zipfile = None
    )

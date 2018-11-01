from state import lookup
from objs import FakeFullyQualifId

def evaluate(func):
    toplevel = lookup(FakeFullyQualifId("", "_toplevel"))
    driver = lookup(FakeFullyQualifId("", "_driver"))

    driver.eval([toplevel, func])

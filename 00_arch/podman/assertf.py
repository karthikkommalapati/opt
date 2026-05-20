"""
Stub for the UBS DSF assertf library.
In production this validates types strictly; here we pass values through for local simulation.
"""

def assert_dict(value):
    if not isinstance(value, dict):
        raise TypeError(f"Expected dict, got {type(value).__name__}")
    return value

def assert_set(value):
    if not isinstance(value, set):
        raise TypeError(f"Expected set, got {type(value).__name__}")
    return value

def assert_tuple(value):
    if not isinstance(value, tuple):
        raise TypeError(f"Expected tuple, got {type(value).__name__}")
    return value

def assert_int(value):
    if not isinstance(value, int):
        raise TypeError(f"Expected int, got {type(value).__name__}")
    return value

def assert_path(value):
    return str(value)

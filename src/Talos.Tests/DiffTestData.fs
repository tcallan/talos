module DiffTestData

module Case1 =
    let original = """
    [
       [
          0,
          1,
          2,
          3
       ],
       [
          "a",
          "b",
          "c"
       ]
    ]
    """

    let patch = """
    [
       {
          "path" : "/0/0",
          "op" : "remove"
       },
       {
          "path" : "/0/0",
          "op" : "remove"
       },
       {
          "value" : null,
          "path" : "/0/0",
          "op" : "add"
       },
       {
          "path" : "/1",
          "value" : true,
          "op" : "add"
       }
    ]
    """

    let expected = """
    [
       [
          null,
          2,
          3
       ],
       true,
       [
          "a",
          "b",
          "c"
       ]
    ]
    """

module Case2 =
    let original = """
    [
        [[null],
         []],
        {"":[-1]},
        {"wut":{"":-1}}
    ]
    """

    let patch = """
    [
        {"op":"replace","path":"/0/0","value":{}},
        {"op":"remove","path":"/0/1"},
        {"op":"replace","path":"/1","value":[]},
        {"op":"add","path":"/2/hello","value":[0]},
        {"op":"replace","path":"/2/wut","value":[false]}
    ]
    """

    let expected = """
    [[{}],[],{"wut":[false],"hello":[0]}]
    """

module Case3 =
    let original = """
    {
        "a": 1
    }
    """

    let patch = """
    [ {"op": "remove", "path": "/missing"}
    ]
    """

    let error = 
        "Cannot delete missing object member at index \"missing\" in pointer \"/missing\""

    let expected = original

module Case4 =
    let original = """
    [ "hello"
    ]
    """

    let patch = """
    [ {"op": "remove", "path": "/1"}
    ]
    """

    let error =
        "Cannot delete missing array member at index 1 in pointer \"/1\""

    let expected = original

module CaseA1 =
    let original = """
    {
        "foo": "bar"
    }
    """

    let patch = """
    [
        {
            "op": "add",
            "path": "/baz",
            "value": "qux"
        }
    ]
    """

    let expected = """
    {
        "baz": "qux",
        "foo": "bar"
    }
    """

module CaseA2 =
    let original = """
    {
        "foo": [
            "bar",
            "baz"
        ]
    }
    """

    let patch = """
    [
        {
            "op": "add",
            "path": "/foo/1",
            "value": "qux"
        }
    ]
    """

    let expected = """
    {
        "foo": [
            "bar",
            "qux",
            "baz"
        ]
    }
    """

module CaseA3 =
    let original = """
    {
        "baz": "qux",
        "foo": "bar"
    }
    """

    let patch = """
    [
        {
            "op": "remove",
            "path": "/baz"
        }
    ]
    """

    let expected = """
    {
        "foo": "bar"
    }
    """

module CaseA4 =
    let original = """
    {
        "foo": [
            "bar",
            "qux",
            "baz"
        ]
    }
    """

    let patch = """
    [
        {
            "op": "remove",
            "path": "/foo/1"
        }
    ]
    """

    let expected = """
    {
        "foo": [
            "bar",
            "baz"
        ]
    }
    """

module CaseA5 =
    let original = """
    {
        "baz": "qux",
        "foo": "bar"
    }
    """

    let patch = """
    [
        {
            "op": "replace",
            "path": "/baz",
            "value": "boo"
        }
    ]
    """

    let expected = """
    {
        "baz": "boo",
        "foo": "bar"
    }
    """

module CaseA6 =
    let original = """
    {
        "foo": {
            "bar": "baz",
            "waldo": "fred"
        },
        "qux": {
            "corge": "grault"
        }
    }
    """

    let patch = """
    [
        {
            "from": "/foo/waldo",
            "op": "move",
            "path": "/qux/thud"
        }
    ]
    """

    let expected = """
    {
        "foo": {
            "bar": "baz"
        },
        "qux": {
            "corge": "grault",
            "thud": "fred"
        }
    }
    """

module CaseA7 =
    let original = """
    {
        "foo": [
            "all",
            "grass",
            "cows",
            "eat"
        ]
    }
    """

    let patch = """
    [
        {
            "from": "/foo/1",
            "op": "move",
            "path": "/foo/3"
        }
    ]
    """

    let expected = """
    {
        "foo": [
            "all",
            "cows",
            "eat",
            "grass"
        ]
    }
    """

module CaseA8 =
    let original = """
    {
        "baz": "qux",
        "foo": [
            "a",
            2,
            "c"
        ]
    }
    """

    let patch = """
    [
        {
            "op": "test",
            "path": "/baz",
            "value": "qux"
        },
        {
            "op": "test",
            "path": "/foo/1",
            "value": 2
        }
    ]
    """

    let expected = """
    {
        "baz": "qux",
        "foo": [
            "a",
            2,
            "c"
        ]
    }
    """

module CaseA9 =
    let original = """
    {
        "baz": "qux"
    }
    """

    let patch = """
    [
        {
            "op": "test",
            "path": "/baz",
            "value": "bar"
        }
    ]
    """

    let error = "Element at \"/baz\" fails test"

module CaseA10 =
    let original = """
    {
        "foo": "bar"
    }
    """

    let patch = """
    [
        {
            "op": "add",
            "path": "/child",
            "value": {
                "grandchild": {}
            }
        }
    ]
    """

    let expected = """
    {
        "child": {
            "grandchild": {}
        },
        "foo": "bar"
    }
    """

module CaseA11 =
    let original = """
    {
        "foo": "bar"
    }
    """

    let patch = """
    [
        {
            "op": "add",
            "path": "/baz",
            "value": "qux",
            "xyz": 123
        }
    ]
    """

    let expected = """
    {
        "baz": "qux",
        "foo": "bar"
    }
    """

module CaseA12 =
    let original = """
    {
        "foo": "bar"
    }
    """

    let patch = """
    [
        {
            "op": "add",
            "path": "/baz/bat",
            "value": "qux"
        }
    ]
    """

    let error = "Cannot insert missing object member at index \"baz\" in pointer \"/baz/bat\""

    let expected = original

module CaseA14 =
    let original = """
    {
        "/": 9,
        "~1": 10
    }
    """

    let patch = """
    [
        {
            "op": "test",
            "path": "/~01",
            "value": 10
        }
    ]
    """

    let expected = """
    {
        "/": 9,
        "~1": 10
    }
    """

module CaseA15 =
    let original = """
    {
        "/": 9,
        "~1": 10
    }
    """

    let patch = """
    [
        {
            "op": "test",
            "path": "/~01",
            "value": "10"
        }
    ]
    """

    let error = "Element at \"/~01\" fails test"

module CaseA16 =
    let original = """
    {
        "foo": [
            "bar"
        ]
    }
    """

    let patch = """
    [
        {
            "op": "add",
            "path": "/foo/-",
            "value": [
                "abc",
                "def"
            ]
        }
    ]
    """

    let expected = """
    {
        "foo": [
            "bar",
            [
                "abc",
                "def"
            ]
        ]
    }
    """

module CaseF1 =
    let original = """
    {
        "foo": {
            "bar": "baz"
        }
    }
    """

    let patch = """
    [
        { "op": "replace", "path": "/foo/bar", "value": "new1"},
        { "op": "replace", "path": "/foo/buz", "value": "new2"}
    ]
    """

    let expected = """
    {
        "foo": {
            "bar": "new1"
        }
    }
    """

    let error = "Cannot delete missing object member at index \"buz\" in pointer \"/foo/buz\""
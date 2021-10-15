from subprocess import Popen, PIPE
import sys
import functools
from typing import List, Dict, Set, Iterable, Union, Optional, Tuple

try:
    import orgparse
except:
    try: 
        from orgparse import orgparse # type: ignore
    except:
        print("No module orgparse: clone it in this folder: https://github.com/karlicoss/orgparse")
        sys.exit(2)

OrgNode = orgparse.OrgNode # for typing
NumOrStr = Union[int, float, str]
ODict = Dict[str, List[NumOrStr]]

def first(it: Iterable):
    if type(it) is list:
         if len(it) > 0: return it[0]
         else: return None
    else: return first(list(it))

def enforce(bool_: bool, msg: str):
    if not bool_: raise Exception(msg)
    else: pass

def empty(it: Union[List, Dict, Set]): return len(it) == 0

def runcmd(cmd_plus_args: Union[str, List[str]], input:Optional[str] = None) -> Tuple[str, str, int]:
    p = Popen(cmd_plus_args, stdout=PIPE, shell=False, text=True)
    (stdout, stderr) = p.communicate(input)
    rc = p.wait()
    return (stdout, stderr, rc)

@functools.lru_cache(maxsize=128)
def checkmd5(expected: str, filename: str) -> bool :
    (stdout, stderr, rc) = runcmd(['md5sum', filename])
    enforce(rc == 0, f"Can't compute md5sum of {filename}")
    return stdout.split(':')[0] == expected

def dump_org(root: OrgNode, keys_: ODict, dst: str, results: Dict[str, NumOrStr]):
    ''' 
    Write entire orgmode tree root to file
    results contains a dict of headers for which we want to (over)write the Result section
    '''
    # keys store values required for each level
        # keys_ = { 
        #     'Source': ['nsamples', 'duration', 'originalfile', 'sourcefile', 'snapshots'],
        #     'Transcoding Parameters': ['scale', 'rencodewithsrc'],
        #     'Scores': []
        # }

    def to_org_table(heading, results):
        assert heading in keys_
        keys = keys_[heading]

        table = ['| K | V |', '|---+---|']
        for k, v in results.items():
            assert  keys == [] or k in keys, f'Invalid key in table generation: {keys}, {k}, {v}'
            str_ = f'| {k} | {v} |'
            table.append(str_)
        return '\n'.join(table)

    def w(fp, node): 
        if node.parent.heading in results and node.heading == 'Result':
            pass # we overwrote this node at the previous iteration
        else:
            fp.write(str(node))

        if node.heading in results:
            fp.write('\n*** Results\n')
            res = to_org_table(node.heading, results)
            fp.write(res)

        fp.write('\n')

        for c in node.children: 
            w(fp, c)

    with open(dst, 'w') as fp: 
        w(fp, root) 

def execute_cmd(action, params):
    '''
    Python is used only to read and write the org file while validating its contents.
    Computation and dealing with ffmpeg is left to OCaml that is called in a subprocess
    IPC uses a simple "DO|<CMD>;<params>" message payload 
    where <CMD> is a the command that calls the right ocaml logic/function and
    <params> is a semicolon divided list of parameters
    '''
    commands = {
        'snapshots': lambda p: f"DO|SNAPSHOTS;{p['originalfile']};{p['nsamples']};{p['duration']}",
        'concat': lambda p: f"DO|MAKESRC;{p['sourcefile']};{';'.join(p['samples'])};",
        'params': lambda p: f"DO|PARAMS;{p['originalfile']}",
        'makedst': lambda p: f"DO|MAKEDST;{p['src']};{p['dst']}",
        'compscore': lambda p: f"DO|COMPSCORE;{p['src']};{p['dst']};{p['logfile']}",
        'analyze': lambda p: f"DO|SCORE;{p['logfile']}"
    }

    st = commands[action](params)
    caml = Popen('main', stdout=PIPE, stdin=PIPE, shell=False, text=True)
    caml.stdin.write(st)
    print(caml.communicate())

def read_properties_of_branch(node: OrgNode) -> Dict[str, NumOrStr]:
    '''
    read and merge the properties of node and its sublevels
    '''
    res = node.properties
    for prop in map(lambda c: c.properties, node.children):

        a_ = map(lambda k: k if k in res else None, prop.keys())
        a = list(filter(lambda n: n is not None, a_))
        enforce(empty(a), f"Duplicate key(s) {a} in level '{node.heading}'")

        res.update(prop)
    return res

def validate_structure(root: OrgNode, required_keys: Set[str], name: str, check_additional=True) -> Dict[str, NumOrStr]:
    '''
    Check that for a section (root) validate and return the org properties in a dict
    '''
    assert root.heading == name, root.heading
    assert root.level == 2, root.level

    properties = read_properties_of_branch(root)

    pk = set(properties.keys())
    if check_additional:
        additional = pk - required_keys
        enforce(empty(additional), f"Unknown keys: {additional} in {name}")
    missing = required_keys - pk
    enforce(empty(missing), f"missing keys: {missing} in {name}")

    return properties

def collect_result(root: OrgNode) -> Dict[str, NumOrStr]:
    '''Parse and collect the table in the Result section'''
    rnode = first(filter(lambda c: c.level == 3 and c.heading == 'RESULT', root.children))
    if rnode is None:
        return {}
    else:
        table = first(rnode.body_rich)
        blocks = list(table.blocks)
        enforce(len(blocks) == 2, f"Mishaped result table in {root.heading}")
        # header = dict(blocks[0])
        values = dict(blocks[1])
        return values

def is_coherent(h: str, properties: Dict[str, NumOrStr], results: Dict[str, NumOrStr]) -> bool:
    '''
    Check that the result table for section h is still valid for the properties that are present.
    '''

    if len(properties.keys() - results.keys()) > 0: return False

    ok = sum(map(lambda k: int(properties[k] == results[k]), properties.keys()))

    # now check md5sum
    md5 = True
    if h == 'Source':
        of = properties['originalfile']; assert isinstance(of, str)
        sf = properties['sourcefile']; assert isinstance(sf, str)
        a = checkmd5(results[of], of)
        b = checkmd5(results[sf], sf)
        md5 = a and b
    elif h == 'Transcoding Parameters':
        pass
    elif h == 'Scores':
        pass
    else:
        assert False, h

    return ok == len(properties.keys()) and md5

def is_complete_results(keys_: ODict, h: str, results: Dict[str, NumOrStr]) -> bool:
    '''
    Check that the values stored in the Result tables is still valid (hash) and what we expect
    '''
    keys = keys_[h]
    if len(set(keys) - results.keys()) > 0: return False

    if h == 'Source' or h == 'Transcoding Parameters':
        for fname, hash_ in results.items():
            if fname.endswith('.mkv') and checkmd5(hash_, fname) == False:
                return False
    elif h == 'Score':
        for fname, hash_ in results.items():
            if fname.endswith('.txt') and checkmd5(hash_, fname) == False:
                return False

    return True

def process_org(root):
    '''
    take the FFBENCH root of the tree and return the next operation that should be done
    '''
    assert root.level == 1 and root.heading == 'FFBENCH'
    ## correctness of the program depends on python respecting insertion order
    keys = { # keys used for properties
        'Source': list(['nsamples', 'duration', 'originalfile', 'sourcefile']),
        'Transcoding Parameters': list(['scale', 'rencodewithsrc']),
        'Scores': list()
    }

    children = root.children
    for c in children:
        heading = c.heading
        enforce(heading in keys.keys(), f"Unknown heading: {heading}")

    for h, keys in keys.items():
        c = first(filter(lambda c: c.heading == h, children))
        properties = validate_structure(c, keys, h, h != 'Transcoding Parameters')
        results = collect_result(c)

    for h, keys in keys.items():
        while True:
            if is_coherent(h, properties, results) and is_complete_results(keys, h, results):
                dump_org()
                break
            else:
                # na = next_action(h, properties, results)
                pass

def main():        
    r = orgparse.load('./result.org')
    for node in r[0:]:
        if node.level == 1 and node.heading == 'FFBENCH':
            return process_org(node)
    enforce(False, "Can't find FFBENCH level")


if __name__ == '__main__':
    main()
    execute_cmd('params', {'originalfile': 'input.mkv'})
    assert False, "Quote filename for shell"

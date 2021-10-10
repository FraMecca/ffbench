from subprocess import Popen, PIPE
import sys

try:
    import orgparse
except:
    try: 
        from orgparse import orgparse
    except:
        print("No module orgparse: clone it in this folder: https://github.com/karlicoss/orgparse")
        sys.exit(2)



print ('-------------')
print ('DONE')
print ('-------------')

# Source
# Transcoding Parameters
# Scores

def first(it):
    if type(it) is list: return it[0]
    else: return first(list(it))

def enforce(bool_, msg):
    if not bool_: raise Exception(msg)
    else: pass

def empty(it): return len(it) == 0

def execute_cmd(action, params):
    commands = {
        'snapshots': lambda p: f"DO|SNAPSHOTS;{p['originalfile']};{p['nsamples']};{p['duration']}",
        'concat': lambda p: f"DO|MAKESRC;{p['sourcefile']};{';'.join(p['samples'])};",
        'params': lambda p: f"DO|PARAMS;{p['originalfile']}",
        'makedst': lambda p: f"DO|MAKEDST;{p['src']};{p['dst']}",
        'score': lambda p: f"DO|SCORE;{p['src']}"
    }

    st = commands[action](params)
    caml = Popen('main', stdout=PIPE, stdin=PIPE, shell=False, text=True)
    caml.stdin.write(st)
    print(caml.communicate())

def read_properties_of_branch(node):
    res = node.properties
    for prop in map(lambda c: c.properties, node.children):

        a = map(lambda k: k if k in res else None, prop.keys())
        a = list(filter(lambda n: n is not None, a))
        enforce(empty(a), f"Duplicate key(s) {a} in level '{node.heading}'")

        res.update(prop)
    return res

def validate_node(root, required_keys, name, check_additional=True):
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


def process_org(root):
    ## correctness of the program depends on python respecting insertion order
    keys = {
        'Source': set(['nsamples', 'duration', 'originalfile', 'sourcefile']),
        'Transcoding Parameters': set(['scale', 'rencodewithsrc']),
        'Scores': set()
    }

    children = root.children
    for c in children:
        heading = c.heading
        enforce(heading in keys.keys(), f"Unknown heading: {heading}")

    for h, keys in keys.items():
        c = first(filter(lambda c: c.heading == h, children))
        properties = validate_node(c, keys, h, h != 'Transcoding Parameters')
        print()
        print(h, properties)
        print()
        
def main():        
    r = orgparse.load('./result.org')
    for node in r[0:]:
        if node.level == 1 and node.heading == 'FFBENCH':
            return process_org(node)
    enforce(False, "Can't find FFBENCH level")


if __name__ == '__main__':
    main()
    execute_cmd('params', {'originalfile': 'input.mkv'})
    assert(False, "Quote filename for shell")

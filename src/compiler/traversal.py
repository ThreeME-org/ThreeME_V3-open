from itertools import product, chain, tee
from collections import Iterable
from copy import deepcopy
from funcy import *
import re
import code
import logging

class AST:
    def __init__(self, nodetype, children):
        self.nodetype = nodetype
        self.children = children
        self.compiled = None
        self.generated = None
        self.as_value = False
        self.override = False
        self.conditions = []

    def __getitem__(self, i):
        return self.children[i]

    def __iter__(self):
        return iter(self.children)

    def __len__(self):
        return len(self.children)

    def __eq__(self, other):
        if isinstance(other, self.__class__):
            return all(selfc == otherc for (selfc, otherc) in zip(self.children, other.children))
        else:
            return False

    def __ne__(self, other):
        return not self.__eq__(other)

    @property
    def is_immediate(self):
        return len(self.children) == 1 and not isinstance(self.children[0], AST) and not self.nodetype in ["loopCounter", "localName"]

    @property
    def is_none(self):
        return self.nodetype == "none"

    @property
    def immediate(self):
        if len(self.children) == 1:
            return self.children[0]
        else:
            raise TypeError

    def __str__(self):
        base = self.nodetype + ": "
        if self.is_immediate:
            return base + repr(self.children[0])
        else:
            return base + '(' + ', '.join([str(e) for e in self.children]) + ')'

ASTNone = AST('none', [])

def clone(obj):
    if isinstance(obj, AST):
        if obj.is_none:
            cloned = ASTNone
        else:
            cloned = AST(obj.nodetype, [clone(c) for c in obj.children])
            cloned.compiled = [clone(c) for c in obj.compiled] if not obj.compiled is None else None
            cloned.generated = [clone(c) for c in obj.generated] if not obj.generated is None else None
            cloned.as_value = obj.as_value
    else:
        cloned = obj
    return cloned

# From http://stackoverflow.com/questions/5286541/how-can-i-flatten-lists-without-splitting-strings
def flatten(foo):
    for x in foo:
        if hasattr(x, '__iter__'):
            for y in flatten(x):
                yield y
        else:
            yield x

def variableNames_not_in_iterators(ast):
    if ast.is_immediate:
        if ast.nodetype == "variableName":
            return ast.immediate
        else:
            return []

    # We must only return variables that are not defined in the formula's iterators
    elif ast.nodetype == "formula":
        var_in_equation = list(variableNames_not_in_iterators(ast.children[1]))
        var_in_iterators = list(flatten(variableNames_not_in_iterators(a) for a in ast.children[3:]))
        return [v for v in var_in_equation if v not in var_in_iterators]

    elif ast.is_none or ast.nodetype in ["loopCounter", "localName"]:
        return []

    else:
        return flatten(variableNames_not_in_iterators(c) for c in ast.children)

def from_heap(key, heap):
    if key in heap:
        return heap[key]
    else:
        raise NameError("Variable '{0}' is not defined".format(key))

def compile_ast(ast, bindings = {}, heap = {}, use_bindings = False, use_heap = False, as_value = False):
    ast.as_value = as_value

    if ast.is_immediate:
        imm = ast.immediate
        # Always use bindings for loop counters
        use_bindings = use_bindings or str(imm)[0] == '$'
        if use_bindings and imm in bindings and ast.nodetype == "variableName":
            ast.compiled = bindings[imm]
        else:
            ast.compiled = imm

    elif ast.nodetype == "loopCounter":
        ast.compiled = bindings[ast.immediate]

    elif ast.nodetype == "localName":
        if use_heap:
            ast.compiled = from_heap(ast.immediate, heap)
        else:
            ast.compiled = ast.children[0]

    elif ast.nodetype in ["formula", "seriesFormula"]:
        # Get all the variable names used in the equation (as strings)
        variableNames = set(variableNames_not_in_iterators(ast))

        # Find the iterators that are in the heap
        # and that are referenced in the equation
        heapIteratorNames = [v for v in variableNames if v in heap and isinstance(heap[v], dict)]

        # But iterators that are already defined
        # in the bindings have priority
        heapIteratorNames = [v for v in heapIteratorNames if v not in bindings]

        # Get these iterators from the heap
        heapIterators = [heap[v] for v in heapIteratorNames]

        # First compile iterators
        if not ast.children[3].is_none or len(heapIteratorNames) > 0:
            if not ast.children[3].is_none:
                iterators = [compile_ast(c, heap = heap, use_heap = True).compiled if c.nodetype == "iterator"
                             else from_heap(c.immediate, heap) for c in ast.children[3:]]
            else:
                iterators = []

            # Names of all the iterators and associated loop counters
            all_names = cat(iter['names'] for iter in iterators)
            # Iterators to be added from the heap
            iterators += [iter for iter in heapIterators if not iter['names'][0] in all_names]
            # Get the lists only
            all_lists = [iter['lists'] for iter in iterators]
            # Cartesian product of all the iterators' lists
            prod = product(*all_lists)
            # Updated names for all iterators, including those from the heap
            all_names = cat(iter['names'] for iter in iterators)
            # Build the final list containing all the bindings
            all_bindings = [dict(zip(all_names, cat(p))) for p in prod]
        else:
            all_bindings = [{}]

        # Apply external bindings, if any
        if len(bindings) > 0:
            all_bindings = [merge(locals, bindings) for locals in all_bindings]

        # Check for the price-value option
        price_value = not ast.children[0].is_none and "@pv" in ast.children[0]

        # Check for the override option
        ast.override = not ast.children[0].is_none and "@over" in ast.children[0]

        # Then compile conditions
        if not ast.children[2].is_none:
            conditions = (compile_ast(clone(ast.children[2]),
                                      bindings = locals,
                                      heap = heap,
                                      use_bindings = True,
                                      use_heap = True) for locals in all_bindings)
            # If price-value is set, should generate a second set of equations - but conditions should remain unchanged, thus we just repeat them
            if price_value:
                conditions = chain(conditions, (compile_ast(clone(ast.children[2]),
                                                            bindings = locals,
                                                            heap = heap,
                                                            use_bindings = True,
                                                            use_heap = True) for locals in all_bindings))
        else:
            conditions = []

        # Finally compile the equation / expression for each binding
        equations = (compile_ast(clone(ast.children[1]),
                                 bindings = locals,
                                 heap = heap,
                                 use_bindings = use_bindings,
                                 use_heap = True,
                                 as_value = as_value) for locals in all_bindings)
        # If price-value is set, should generate a second set of equations, in value form
        if price_value:
            equations = chain(equations, (compile_ast(clone(ast.children[1]),
                                                      bindings = locals,
                                                      heap = heap,
                                                      use_bindings = use_bindings,
                                                      use_heap = True,
                                                      as_value = True) for locals in all_bindings))

        ast.compiled = { 'conditions': list(conditions),
                         'equations': list(equations) }

    elif ast.nodetype == "function":
        name = compile_ast(ast.children[0]).compiled

        if name == "sum":
            generator = lambda toks, heap: "(0 + " + ' + '.join(toks) +")" if len(toks) > 0 else "0"

        elif name == "value":
            generator = lambda toks, heap: toks[0]
            # We force all the arguments to be generated as values
            as_value = True

        elif name == "if":
            generator = lambda toks, heap: toks[1] if eval(toks[0], heap) else toks[2]

        else:
            generator = lambda toks, heap: ast.compiled['name'] + '(' + ', '.join(toks) + ')'

        ast.compiled = { 'name': name,
                         'generator': generator,
                         'arguments': [compile_ast(c,
                                                   bindings = bindings,
                                                   heap = heap,
                                                   use_bindings = True,
                                                   use_heap = True,
                                                   as_value = as_value) for c in ast.children[1:]] }

    elif ast.nodetype in ["index", "placeholder", "timeOffset"]:
        ast.compiled = [compile_ast(c,
                                    bindings = bindings,
                                    heap = heap,
                                    use_bindings = True,
                                    use_heap = True) for c in ast.children]

    elif ast.nodetype in ["identifier", "array"]:
        # The as_value flag should not propagate downwards inside the identifier and array nodetypes
        if as_value:
            as_value = False
        ast.compiled = [compile_ast(c,
                                    bindings = bindings,
                                    heap = heap,
                                    use_bindings = use_bindings,
                                    use_heap = True,
                                    as_value = as_value) for c in ast.children]

    elif ast.nodetype == "iterator":
        names = ast.children[0].children
        lists = ast.children[1].children

        if isinstance(lists, basestring):
            lists = [lists]
        # If the iterator is correctly defined, there are as many iterator names
        # as there are lists.
        if len(names) <> len(lists):
            raise SyntaxError("An iterator must have the same number of list names and list definitions")
        compiled_names = [compile_ast(c).compiled for c in ast.children[0].children]
        compiled_lists = [compile_ast(c, heap = heap, use_heap = True).compiled for c in ast.children[1].children]

        # Ensure that all elements of compiled lists really are lists
        # This prevents bugs when a list is actually a singleton
        compiled_lists = [ {'list': [h], 'loopCounters': [1]} if isinstance(h, basestring)
                           else h for h in compiled_lists ]
        compiled_lists = [ [h['list'], h['loopCounters']] for h in compiled_lists]
        # Add the loop counter names
        compiled_names = cat(zip(compiled_names, ['$' + name for name in compiled_names]))
        compiled_lists = reduce(lambda x, y: x + y, compiled_lists, [])
        ast.compiled = { 'names': compiled_names,
                         'lists': zip(*compiled_lists) }

    elif ast.nodetype == "listBase":
        ast.compiled = [compile_ast(c).compiled for c in ast.children]

    elif ast.nodetype == "list":
        base = compile_ast(ast.children[0], heap = heap, use_heap = True).compiled
        excluded = compile_ast(ast.children[1], heap = heap, use_heap = True).compiled

        # If the lists were defined as localNames,
        # base and excluded are already compiled lists,
        # not listBases.
        # Must extract the lists themselves
        if isinstance(base, dict) and 'list' in base:
            base = base['list']
        if isinstance(excluded, dict) and 'list' in excluded:
            excluded = excluded['list']

        # If base or excluded are strings (single-element lists)
        if isinstance(base, basestring):
            base = [base]
        if isinstance(excluded, basestring):
            excluded = [excluded]

        # WARNING ! Loop counters are based on the index of the base list
        # This is because loop counters are often used
        # to refer to columns in the calibration matrices
        # When an item is excluded from the base list, it should not
        # shift all the other columns
        loopCounters = range(1, len(base) + 1)
        ast.compiled = { 'list' : [e for e in base if e not in excluded],
                         'loopCounters': [i for e, i in zip(base, loopCounters) if e not in excluded] }

    elif ast.is_none:
        ast.compiled = ASTNone

    else:
        ast.compiled = [compile_ast(c,
                                    bindings = bindings,
                                    heap = heap,
                                    use_bindings = use_bindings,
                                    use_heap = use_heap,
                                    as_value = as_value) for c in ast.children]

    return ast


def generated_variables(ast):
    if isinstance(ast, AST):
        # Formula inside a function, all the terms must be combined
        if ast.nodetype == "formula":
            return cat([generated_variables(eq) for eq in ast.compiled["equations"]] +
                       [generated_variables(cond) for cond in ast.compiled["conditions"]] )
        # In a function, we only want the arguments
        elif ast.nodetype in ["function"]:
            return cat([generated_variables(c) for c in ast.children[1:]])
        elif ast.nodetype in ["variableName", "identifier", "identifierTime", "array"]:
            if ast.generated[0] <> '@' and not ast.generated.isdigit():
                out = ast.generated.upper()
                # We remove time offsets
                out = re.sub(r"\(.+?\)", "", out)
                # We split terms expressed as value
                if ast.as_value:
                    return [out.split('*')[0].strip(), out.split('*')[1].strip()]
                else:
                    return [out]
            else:
                return []
        # Placeholders and indices are internal to the compiler, and can be safely skipped
        elif ast.nodetype in ["placeholder", "index"]:
            return []
        elif ast.is_immediate or ast.is_none or ast is None:
            return []
        else:
            return cat([generated_variables(c) for c in ast.children])
    else:
        return []


def dependencies(ast):
    dep = {}
    if isinstance(ast, AST):
        if ast.nodetype == "instruction":
            return dependencies(ast.children[0])
        elif ast.nodetype in ["assignment", "iterator"]:
            return {}
        # Can only be called on a top-level formula or seriesFormula node (ie not included inside a function)
        elif ast.nodetype in ["formula", "seriesFormula"] and ast.children[1].nodetype == "equation":
            # When compiling the model, we filter by condition
            if ast.nodetype == "formula":
                compiled_equations = [eq for eq, cond in zip(ast.compiled['equations'], ast.conditions) if cond]
            # When compiling data calibration, that's not necessary
            # (and not possible since conditions are not checked at compile-time for calibration)
            else:
                compiled_equations = ast.compiled['equations']

            for gen_eq, eq in zip(ast.generated, compiled_equations):
                lvar = generated_variables(eq.children[0])
                rvar = generated_variables(eq.children[1])
                # Check for variable as value on the left-hand side
                #if '*' in lvar[0]:
                dep[lvar[0]] = {'equation': gen_eq, 'dependencies': cat([lvar[1:], rvar])}
            return dep
    elif ast == "":
        return {}
    else:
        if isinstance(ast, AST):
            raise TypeError("Cannot direcly compute dependencies of a {0} node".format(ast.nodetype))
        else:
            raise TypeError("Cannot compute dependencies of a {0}".format(ast.__class__.__name__))

def value_form(str, flag):
    if flag:
        return 'P' + str + ' * ' + str
    else:
        return str


def generate(ast, heap = {}):
    if ast.is_immediate or ast.nodetype in ["loopCounter", "localName"]:
        ret = str(ast.compiled)
        if ast.nodetype == "variableName":
            ast.generated = value_form(ret, ast.as_value)
        else:
            ast.generated = ret

    elif ast.nodetype == "variable":
        ast.generated = generate(ast.children[0])[0].generated

    elif ast.nodetype == "array":
        ret = generate(ast.children[0])[0].generated
        if not ast.children[1].is_none:
            ret += '_' + generate(ast.children[1])[0].generated
        if not ast.children[2].is_none:
            ret += generate(ast.children[2])[0].generated
        ast.generated = value_form(ret, ast.as_value)

    elif ast.nodetype in ["identifier", "identifierTime"]:
        ast.generated = value_form(''.join(generate(c)[0].generated for c in ast.compiled), ast.as_value)

    elif ast.nodetype == "condition":
        # All variables in the heap should be uppercase
        ast.generated = generate(ast.compiled[0])[0].generated.upper()

    elif ast.nodetype == "expression":
        ast.generated = ' '.join(generate(c, heap)[0].generated for c in ast.compiled)

    elif ast.nodetype == "equation":
        ast.generated = generate(ast.children[0], heap)[0].generated + ' = ' + generate(ast.children[1], heap)[0].generated

    elif ast.nodetype == "formula":
        conditions = [generate(cond, heap)[0].generated for cond in ast.compiled['conditions']]
        equations = [generate(eq, heap)[0].generated for eq in ast.compiled['equations']]
        if len(conditions) > 0:
            ast.conditions = [eval(cond, heap) for cond in conditions]
            ast.generated = [eq for eq, cond in zip(equations, ast.conditions) if cond]
        else:
            ast.conditions = [True] * len(equations)
            ast.generated = equations

    elif ast.nodetype == "seriesFormula":
        conditions = [generate(cond, heap)[0].generated for cond in ast.compiled['conditions']]
        equations  = [generate(eq, heap)[0].generated for eq in ast.compiled['equations']]
        if len(conditions) > 0:
            # !!! HORRIBLE HACK: we suppose that all conditions are of the form
            # 'Series > 0' or 'Series <> 0'
            # In these cases, they are transformed into '@elem(Series)  > 0' or '@elem(Series) <> 0'
            ast.generated = ["if @elem({0}, \"{3}\") {1} then\n  series {2}\nendif".format(cond[:-4], cond[-4:], eq, heap['%baseyear'])
                             for eq, cond in zip(equations, conditions)]
        else:
            ast.generated = ["series {0}".format(eq) for eq in equations]

    elif ast.nodetype == "function":
        generated_args = [generate(a, heap)[0].generated for a in ast.compiled['arguments']]
        # Special case if there is only one argument, parsed as a formula but behaving like an expression
        if len(generated_args) == 1 and isinstance(generated_args[0], list):
            generated_args = generated_args[0]
        ast.children[1].generated = generated_args
        ast.generated = ast.compiled['generator'](generated_args, heap)

    elif ast.nodetype == "placeholder":
        ast.generated = ''.join(generate(c)[0].generated for c in ast.compiled)

    elif ast.nodetype == "index":
        ast.generated = '_'.join(generate(c, heap)[0].generated for c in ast.compiled)

    elif ast.nodetype == "timeOffset":
        ast.generated = '(' + generate(ast.children[0])[0].generated + ')'

    elif ast.nodetype == "assignment":
        ast.generated = ""
        for (localName, value) in zip(ast.children[0].compiled, ast.children[1].compiled):
            if len(value.compiled['list']) == 1:
                heap[localName.compiled] = value.compiled['list'][0]
            else:
                heap[localName.compiled] = value.compiled

    elif ast.nodetype == "instruction":
        child = ast.children[0]
        if child.nodetype == "iterator":
            # If a single iterator is defined, then :names contains
            # only the iterator name, and its associated loop counter
            if len(child.compiled["names"]) > 2:
                raise SyntaxError("Parallel iterators are not allowed in global iterator definitions")
            heap[child.compiled["names"][0]] = child.compiled
            ast.generated = ""
        else:
            generated_instruction, heap = generate(child, heap)
            ast.override = generated_instruction.override
            ast.generated = generated_instruction.generated

    elif ast.is_none:
        ast.generated = ""

    return ast, heap

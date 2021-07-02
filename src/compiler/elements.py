from collections import *
from funcy import *

import itertools

def priceVolume(base, option):
    if option == '!pv':
        return 'P' + base + ' * ' + base
    else:
        return base

class BaseElement(namedtuple("BaseElement", ['value'])):
    def compile(self, bindings, heap, option):
        return str(self.value)

# Used to mark parsed elements that contain immediate (ie constant) values
class Immediate: pass

# Numerical types
class Integer(BaseElement, Immediate): pass
class Real(BaseElement, Immediate): pass

# A VariableName must start with an alphabetical character or an underscore,
# and can contain any number of alphanumerical characters or underscores
class VariableName(BaseElement):
    def getLoopCounterVariable(self):
        return VariableName('$' + self.value)

    def compile(self, bindings, heap, option):
        if self in bindings.keys():
            return str(bindings[self])
        else:
            return priceVolume(str(self.value), option)

# A Placeholder is a VariableName enclosed in curly brackets, e.g. `{X}`
class Placeholder(BaseElement):
    def compile(self, bindings, heap, option):
        return bindings[self.value]

class HasIteratedVariables:
    def getIteratedVariableNames(self): raise NotImplementedError

# An identifier is a combination of one or more VariableNames and Placeholders
# e.g. {V}_energy, or Price{O}
class Identifier(BaseElement, HasIteratedVariables):
    def getIteratedVariableNames(self):
        return [e.value for e in self.value if isinstance(e, Placeholder)]

    def compile(self, bindings, heap, option):
        # VariableNames and Placeholders composing the Identifier must be compiled
        # without the price-volume option, if any
        return priceVolume(''.join([e.compile(bindings, heap, '') for e in self.value]), option)

# An Index is used in an Array to address its individual elements
# It can have multiple dimensions, e.g. [com, sec]
class Index(BaseElement, HasIteratedVariables):
    def getIteratedVariableNames(self):
        return cat([e.getIteratedVariableNames() for e in self.value])

    def compile(self, bindings, heap, option):
        return '_'.join([e.compile(bindings, heap, option) for e in self.value])

class TimeOffset(BaseElement):
    def compile(self, bindings, heap, option):
        return '(' + self.value.compile(bindings, heap, option) + ')'

# An Array is a combination of a Identifier and an Index
class Array(namedtuple("Array", ['identifier', 'index', 'timeOffset']), HasIteratedVariables):
    def getIteratedVariableNames(self):
        return self.identifier.getIteratedVariableNames() + self.index.getIteratedVariableNames()

    def compile(self, bindings, heap, option):
        # Components of the Array must be compiled
        # without the price-volume option, if any
        timeOffset = self.timeOffset[0].compile(bindings, heap, option) if len(self.timeOffset) > 0 else ''
        return priceVolume(self.identifier.compile(bindings, heap, '') + '_' + self.index.compile(bindings, heap, ''), option) + timeOffset

# An Expression is the building block of an equation
# Expressions can include operators, functions and any operand (Array, Identifier, or number)
class Expression(namedtuple("Expression", ['value']), HasIteratedVariables):
    def getIteratedVariableNames(self):
        return cat([e.getIteratedVariableNames() for e in self.value if isinstance(e, HasIteratedVariables)])

    def compile(self, bindings, heap, option):
        return ' '.join([e.compile(bindings, heap, option) for e in self.value])

    def evaluate(self, bindings, heap):
        return eval(' '.join([e.compile(bindings, heap, '') if isinstance(e, Immediate) else
                              str(heap[e.compile(bindings, heap, '').upper()]) for e in self.value]))

class Operator(BaseElement, Immediate): pass

class ComparisonOperator(BaseElement, Immediate): pass

class BooleanOperator(BaseElement, Immediate): pass

class SumFunc(namedtuple("SumFunc", ['formula']), HasIteratedVariables):
    def getIteratedVariableNames(self):
        return set(self.formula.iterated_variables()) - set(self.formula.iterator_variables())

    def compile(self, bindings, heap, option):
        compiled_sum = self.formula.compile_sum(bindings, heap, option)
        if len(compiled_sum) > 0:
            return "0 + " + compiled_sum
        else:
            return "0"

class Func(namedtuple("Func", ['variableName', 'expressions']), HasIteratedVariables):
    def getIteratedVariableNames(self):
        return cat([e.getIteratedVariableNames() for e in self.expressions])

    def compile(self, bindings, heap, option):
        # The 'value' function allows to transform a volume
        # expression into a value expression
        if self.variableName.value == 'value':
            return self.expressions[0].compile(bindings, heap, '!pv')
        # Expressions inside a function (such as, e.g. a dlog) mustn't be compiled
        # with the price-value option, if any
        else:
            return self.variableName.compile({}, {}, '') + '(' + ', '.join([e.compile(bindings, heap, '') for e in self.expressions]) + ')'

# An Equation is made of two Expressions separated by an equal sign
class Equation(namedtuple("Equation", ['lhs', 'rhs']), HasIteratedVariables):
    def getIteratedVariableNames(self):
        return self.lhs.getIteratedVariableNames() + self.rhs.getIteratedVariableNames()

    def compile(self, bindings, heap, option):
        volumeEquation = self.lhs.compile(bindings, heap, '') + ' = ' + self.rhs.compile(bindings, heap, '')
        if option == '!pv':
            priceEquation = self.lhs.compile(bindings, heap, option) + ' = ' + self.rhs.compile(bindings, heap, option)
            return priceEquation + '\n' + volumeEquation
        # If no options have been specified
        else:
            return volumeEquation

class Condition(namedtuple("Condition", ["expression"]), HasIteratedVariables):
    def getIteratedVariableNames(self):
        return self.expression.getIteratedVariableNames()

    def evaluate(self, bindings, heap):
        return self.expression.evaluate(bindings, heap)

# A Lst is a sequence of space-delimited strings (usually numbers), used for an iterator
# e.g. 01 02 03 04 05 06
class Lst(namedtuple("LstBase", ['base', 'remove'])):
    def compile(self):
        return [e for e in self.base if e not in self.remove]

# An Iterator is the combination of a VariableName and a Lst
# Each occurence of the VariableName inside an Index or a Placeholder will be replaced
# with each value in the Lst, sequentially, at the compile stage
# e.g. com in 01 02 03 04 05 06 07 08 09
class Iter(namedtuple("Iter", ['variableName', 'lst'])):
    def compileLoopCounter(self):
        # WARNING: the range of the loop counter is calculated over the base list, not the copmiled list
        # This is because the list removal feature is designed to skip an equation,
        # but the loop counter is usually used to iterate over rows or columns of data
        # which ignore this skipping
        return {self.variableName.getLoopCounterVariable(): range(1, len(self.lst.base) + 1) }

    # Return a dict of: {VariableName: compiled Lst}
    def compile(self):
        return {self.variableName: self.lst.compile()}

# A Formula is the combination of an Equation, zero or one Condition, and one or more Iter(ators)
# This is the full form of the code passed from eViews to the compiler
# e.g. {V}[com] = {V}D[com] + {V}M[com], V in Q CH G I DS, com in 01 02 03 04 05 06 07 08 09
class Formula(namedtuple("Formula", ['options', 'equation', 'conditions', 'iterators'])):
    def iterator_variables(self):
        return [i.variableName for i in self.iterators]

    def iterated_variables(self):
        return self.equation.getIteratedVariableNames()

    def cartesianProduct(self, iterators):
        # Cartesian product of all iterators, returned as dicts
        # Turns {'V': ['Q', 'X'], 'com': ['01', '02', '03']}
        # into [{'V': 'Q', 'com': '01'}, {'V': 'Q', 'com': '02'}, {'V': 'Q', 'com': '03'},
        #       {'X': 'Q', 'com': '01'}, {'X': 'Q', 'com': '02'}, {'X': 'Q', 'com': '03'} ]
        cartesianProd = [l for l in apply(itertools.product, iterators.values())]
        return [dict(zip(iterators.keys(), p)) for p in cartesianProd]

    def build_iterator_dicts(self):
        # Check that each iterator is defined only once
        if len(self.iterator_variables()) > len(set(self.iterator_variables())):
            raise NameError("Some iterated variables are defined multiple times")

        # Compile each iterator to get a dict of {VariableNames: Iter}
        iterators = dict(cat([i.compile().items() for i in self.iterators]))
        loopCounters = dict(cat([i.compileLoopCounter().items() for i in self.iterators]))

        iteratorDicts = self.cartesianProduct(iterators)
        loopCounterDicts = self.cartesianProduct(loopCounters)

        return [merge(iterDict, counterDict) for iterDict, counterDict in zip(iteratorDicts, loopCounterDicts)]

    def evaluate_conditions(self, bindings, heap, iteratorDicts):
        # Evaluate the condition for each iterator binding
        if len(self.conditions) > 0:
            if len(self.iterators) > 0:
                conditions = [self.conditions[0].evaluate(dict(bindings.items() + local_bindings.items()), heap)
                              for local_bindings in iteratorDicts]
            else:
                conditions = [self.conditions[0].evaluate(bindings, heap)]
        else:
            if len(self.iterators) > 0:
                conditions = [True] * len(iteratorDicts)
            else:
                conditions = [True]
        return conditions

    def init_compilation(self, bindings, heap):
        iteratorDicts = self.build_iterator_dicts()
        conditions = self.evaluate_conditions(bindings, heap, iteratorDicts)
        option = self.options[0].lower() if len(self.options) > 0 else ''
        return iteratorDicts, conditions, option

    def compile_sum(self, bindings, heap, option):
        iteratorDicts, conditions, _ = self.init_compilation(bindings, heap)
        return " + ".join([self.equation.compile(dict(local_bindings.items() + bindings.items()), heap, option)
                           for condition, local_bindings in zip(conditions, iteratorDicts) if condition])

    def compile(self, heap):
        iteratorDicts, conditions, option = self.init_compilation({}, heap)

        # Check that all VariableNames used as iterators in the equation are defined
        # in the iterators section of the Formula
        missingVars = set(self.iterated_variables()) - set(self.iterator_variables())
        if len(missingVars) > 0:
            raise IndexError("These iterated variables are not defined: " + ", ".join([e.value for e in missingVars]))

        return "\n".join([self.equation.compile(bindings, heap, option)
                          for condition, bindings in zip(conditions, iteratorDicts) if condition])

class SemanticAnalyzer:
    def __init__(self, ast):
        self.ast = ast
        self.symbol_table = {}
        self.errors = []

    def analyze(self):
        self.visit(self.ast)

    def visit(self, node):
        method_name = 'visit_' + node.type
        method = getattr(self, method_name, self.generic_visit)
        return method(node)

    def generic_visit(self, node):
        for child in node.children:
            self.visit(child)

    def visit_PROGRAM(self, node):
        self.generic_visit(node)

    def visit_NUMBER(self, node):
        return 'int'

    def visit_ID(self, node):
        var_name = node.value
        if var_name in self.symbol_table:
            return self.symbol_table[var_name]
        else:
            self.errors.append(f"Undeclared variable: {var_name}")
            return None

    def visit_ASSIGN(self, node):
        var_name = node.children[0].value
        var_type = self.visit(node.children[1])
        self.symbol_table[var_name] = var_type

    def visit_PLUS(self, node):
        left_type = self.visit(node.children[0])
        right_type = self.visit(node.children[1])
        if left_type == right_type == 'int':
            return 'int'
        else:
            self.errors.append("Type error in addition operation")
            return None

    # Similar methods for other operators like MINUS, MULT, DIV, etc.

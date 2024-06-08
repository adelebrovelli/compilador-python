class ASTNode:
    def __init__(self, type, value=None):
        self.type = type
        self.value = value
        self.children = []

    def add_child(self, node):
        self.children.append(node)

class Parser:
    def __init__(self, tokens):
        self.tokens = tokens
        self.pos = 0

    def parse(self):
        return self.program()

    def program(self):
        node = ASTNode('PROGRAM')
        while self.pos < len(self.tokens):
            node.add_child(self.statement())
        return node

    def statement(self):
        node = self.expression()
        self.match('SEMICOLON')
        return node

    def expression(self):
        node = self.term()
        while self.current_token() in ('PLUS', 'MINUS'):
            op = self.current_token()
            self.match(op)
            node = ASTNode(op, node, self.term())
        return node

    def term(self):
        node = self.factor()
        while self.current_token() in ('MULT', 'DIV'):
            op = self.current_token()
            self.match(op)
            node = ASTNode(op, node, self.factor())
        return node

    def factor(self):
        token = self.current_token()
        if token == 'LPAREN':
            self.match('LPAREN')
            node = self.expression()
            self.match('RPAREN')
            return node
        elif token == 'NUMBER':
            self.match('NUMBER')
            return ASTNode('NUMBER', token)
        elif token == 'ID':
            self.match('ID')
            return ASTNode('ID', token)
        else:
            raise SyntaxError(f"Unexpected token: {token}")

    def current_token(self):
        return self.tokens[self.pos][0]

    def match(self, token_type):
        if self.current_token() == token_type:
            self.pos += 1
        else:
            raise SyntaxError(f"Expected token {token_type}, got {self.current_token()}")

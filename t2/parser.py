import ply.yacc as yacc
from lexer import tokens

# Definição das regras de precedência dos operadores
precedence = (
    ('left', 'OR'),
    ('left', 'AND'),
    ('left', 'EQ', 'NE'),
    ('left', 'LT', 'LE', 'GT', 'GE'),
    ('left', 'PLUS', 'MINUS'),
    ('left', 'TIMES', 'DIV', 'MOD'),
    ('right', 'NOT'),
    ('right', 'UMINUS'),
    ('right', 'INCREMENT', 'DECREMENT')
)

# Estrutura para a tabela de símbolos
class SymbolTable:
    def __init__(self):
        self.symbols = {}
        self.scopes = [{}]

    def enter_scope(self):
        self.scopes.append({})

    def exit_scope(self):
        self.scopes.pop()

    def declare(self, name, symbol_type):
        self.scopes[-1][name] = symbol_type

    def lookup(self, name):
        for scope in reversed(self.scopes):
            if name in scope:
                return scope[name]
        return None

symbol_table = SymbolTable()

def check_type_expression(expr, symbol_table):
    if isinstance(expr, tuple):
        if expr[0] == 'atribuicao':
            var_type = symbol_table.lookup(expr[1])
            expr_type = check_type_expression(expr[2], symbol_table)
            if var_type != expr_type:
                raise TypeError(f"Type error: cannot assign {expr_type} to {var_type}")
            return var_type
        elif expr[0] in ('PLUS', 'MINUS', 'TIMES', 'DIV'):
            left_type = check_type_expression(expr[1], symbol_table)
            right_type = check_type_expression(expr[2], symbol_table)
            if left_type != right_type:
                raise TypeError(f"Type error: incompatible types {left_type} and {right_type}")
            return left_type
        # Adicione verificações para outros tipos de expressões
    elif isinstance(expr, int):
        return 'int'
    elif isinstance(expr, float):
        return 'float'
    elif isinstance(expr, str):
        return 'string'
    else:
        return symbol_table.lookup(expr)

def check_declaration(decl, symbol_table):
    if decl[0] == 'declaracao_variavel':
        var_name = decl[2]
        var_type = decl[1]
        symbol_table.declare(var_name, var_type)
        if len(decl) == 4:
            expr_type = check_type_expression(decl[3], symbol_table)
            if var_type != expr_type:
                raise TypeError(f"Type error: cannot assign {expr_type} to {var_type}")
    elif decl[0] == 'declaracao_funcao':
        check_function_declaration(decl, symbol_table)

def check_function_declaration(func_decl, symbol_table):
    func_name = func_decl[1]
    func_type = func_decl[0]
    params = func_decl[2]
    body = func_decl[3]

    symbol_table.enter_scope()
    for param in params:
        symbol_table.declare(param[1], param[0])
    check_block(body, symbol_table)
    symbol_table.exit_scope()

def check_block(block, symbol_table):
    for stmt in block:
        check_declaration(stmt, symbol_table)

def p_programa(p):
    '''programa : declaracao_list'''
    p[0] = ('programa', p[1])
    for decl in p[1]:
        check_declaration(decl, symbol_table)

def p_declaracao_list(p):
    '''declaracao_list : declaracao_list declaracao
                       | declaracao'''
    if len(p) == 3:
        p[0] = p[1] + [p[2]]
    else:
        p[0] = [p[1]]

def p_declaracao(p):
    '''declaracao : declaracao_variavel
                  | declaracao_funcao
                  | declaracao_estrutura
                  | estrutura_controle
                  | comentario
                  | expressao'''
    p[0] = p[1]

def p_declaracao_variavel(p):
    '''declaracao_variavel : tipo ID SEMICOLON
                           | tipo ID EQUALS expressao SEMICOLON'''
    if len(p) == 4:
        p[0] = ('declaracao_variavel', p[1], p[2])
    else:
        p[0] = ('declaracao_variavel', p[1], p[2], p[4])

def p_tipo(p):
    '''tipo : INT
            | FLOAT
            | DOUBLE
            | CHAR
            | BOOLEAN
            | VOID'''
    p[0] = p[1]

def p_declaracao_funcao(p):
    '''declaracao_funcao : tipo ID LPAREN parametros RPAREN bloco'''
    p[0] = ('declaracao_funcao', p[1], p[2], p[4], p[6])

def p_parametros(p):
    '''parametros : parametro
                  | parametro COMMA parametros
                  | vazio'''
    if len(p) == 2:
        p[0] = [p[1]]
    elif len(p) == 4:
        p[0] = [p[1]] + p[3]
    else:
        p[0] = []

def p_parametro(p):
    '''parametro : tipo ID
                 | tipo ID LBRACKET RBRACKET
                 | tipo DOTS ID'''
    if len(p) == 3:
        p[0] = ('parametro', p[1], p[2])
    elif len(p) == 5:
        p[0] = ('parametro_array', p[1], p[2])
    else:
        p[0] = ('parametro_variadico', p[1], p[3])

def p_bloco(p):
    '''bloco : LBRACE declaracao_list RBRACE'''
    p[0] = ('bloco', p[2])

def p_comentario(p):
    '''comentario : COMMENT
                  | MULTI_COMMENT'''
    p[0] = ('comentario',)

def p_expressao(p):
    '''expressao : atribuicao
                 | expressao_logica
                 | chamada_funcao'''
    p[0] = p[1]

def p_atribuicao(p):
    '''atribuicao : ID EQUALS expressao
                  | ID PLUS_EQUALS expressao
                  | ID MINUS_EQUALS expressao
                  | ID TIMES_EQUALS expressao
                  | ID DIV_EQUALS expressao
                  | ID MOD_EQUALS expressao
                  | ID AND_EQUALS expressao
                  | ID OR_EQUALS expressao'''
    if p[2] == '=':
        p[0] = ('atribuicao', p[1], p[3])
    else:
        p[0] = ('atribuicao_composta', p[1], p[2], p[3])

def p_chamada_funcao(p):
    '''chamada_funcao : PRINTLN LPAREN TEXTO RPAREN SEMICOLON
                      | ID LPAREN argumentos RPAREN SEMICOLON'''
    if p[1] == 'println':
        p[0] = ('println', p[3])
    else:
        p[0] = ('chamada_funcao', p[1], p[3])

def p_estrutura_controle(p):
    '''estrutura_controle : IF LPAREN expressao RPAREN bloco
                          | IF LPAREN expressao RPAREN bloco ELSE bloco
                          | WHILE LPAREN expressao RPAREN bloco
                          | FOR LPAREN expressao SEMICOLON expressao SEMICOLON expressao RPAREN bloco
                          | SWITCH LPAREN expressao RPAREN case_lista
                          | BREAK SEMICOLON
                          | CONTINUE SEMICOLON
                          | RETURN expressao SEMICOLON'''
    if p[1] == 'if' and len(p) == 6:
        p[0] = ('if', p[3], p[5])
    elif p[1] == 'if':
        p[0] = ('if_else', p[3], p[5], p[7])
    elif p[1] == 'while':
        p[0] = ('while', p[3], p[5])
    elif p[1] == 'for':
        p[0] = ('for', p[3], p[5], p[7], p[9])
    elif p[1] == 'switch':
        p[0] = ('switch', p[3], p[5])
    elif p[1] == 'break':
        p[0] = ('break',)
    elif p[1] == 'continue':
        p[0] = ('continue',)
    else:
        p[0] = ('return', p[2])

def p_case_lista(p):
    '''case_lista : case_decl
                  | case_decl case_lista'''
    if len(p) == 2:
        p[0] = [p[1]]
    else:
        p[0] = [p[1]] + p[2]

def p_case_decl(p):
    '''case_decl : CASE expressao COLON bloco
                 | DEFAULT COLON bloco'''
    if p[1] == 'case':
        p[0] = ('case', p[2], p[4])
    else:
        p[0] = ('default', p[3])

def p_declaracao_estrutura(p):
    '''declaracao_estrutura : STRUCT ID LBRACE declaracao_variavel_list RBRACE SEMICOLON'''
    p[0] = ('declaracao_estrutura', p[2], p[4])

def p_declaracao_variavel_list(p):
    '''declaracao_variavel_list : declaracao_variavel_list declaracao_variavel
                                | declaracao_variavel'''
    if len(p) == 3:
        p[0] = p[1] + [p[2]]
    else:
        p[0] = [p[1]]

def p_array(p):
    '''array : ID LBRACKET expressao RBRACKET
             | ID LBRACKET RBRACKET'''
    if len(p) == 5:
        p[0] = ('array', p[1], p[3])
    else:
        p[0] = ('array', p[1])

def p_array_inicializacao(p):
    '''array_inicializacao : LBRACE expressao_lista RBRACE'''
    p[0] = ('array_inicializacao', p[2])

def p_expressao_lista(p):
    '''expressao_lista : expressao
                       | expressao COMMA expressao_lista'''
    if len(p) == 2:
        p[0] = [p[1]]
    else:
        p[0] = [p[1]] + p[3]

def p_expressao_logica(p):
    '''expressao_logica : expressao_relacional
                        | expressao_logica AND expressao_relacional
                        | expressao_logica OR expressao_relacional
                        | NOT expressao_relacional'''
    if len(p) == 2:
        p[0] = p[1]
    elif p[1] == '!':
        p[0] = ('not', p[2])
    else:
        p[0] = (p[2], p[1], p[3])

def p_expressao_relacional(p):
    '''expressao_relacional : expressao_aritmetica
                            | expressao_aritmetica LT expressao_aritmetica
                            | expressao_aritmetica LE expressao_aritmetica
                            | expressao_aritmetica GT expressao_aritmetica
                            | expressao_aritmetica GE expressao_aritmetica
                            | expressao_aritmetica EQ expressao_aritmetica
                            | expressao_aritmetica NE expressao_aritmetica'''
    if len(p) == 2:
        p[0] = p[1]
    else:
        p[0] = (p[2], p[1], p[3])

def p_expressao_aritmetica(p):
    '''expressao_aritmetica : expressao_multiplicativa
                            | expressao_aritmetica PLUS expressao_multiplicativa
                            | expressao_aritmetica MINUS expressao_multiplicativa'''
    if len(p) == 2:
        p[0] = p[1]
    else:
        p[0] = (p[2], p[1], p[3])

def p_expressao_multiplicativa(p):
    '''expressao_multiplicativa : expressao_unaria
                                | expressao_multiplicativa TIMES expressao_unaria
                                | expressao_multiplicativa DIV expressao_unaria
                                | expressao_multiplicativa MOD expressao_unaria'''
    if len(p) == 2:
        p[0] = p[1]
    else:
        p[0] = (p[2], p[1], p[3])

def p_expressao_unaria(p):
    '''expressao_unaria : expressao_postfix
                        | MINUS expressao_unaria %prec UMINUS
                        | INCREMENT expressao_postfix
                        | DECREMENT expressao_postfix'''
    if len(p) == 2:
        p[0] = p[1]
    elif p[1] == '-':
        p[0] = ('uminus', p[2])
    else:
        p[0] = (p[1], p[2])

def p_expressao_postfix(p):
    '''expressao_postfix : primaria
                         | primaria LBRACKET expressao RBRACKET
                         | primaria LPAREN argumentos RPAREN
                         | primaria DOT ID
                         | primaria ARROW ID'''
    if len(p) == 2:
        p[0] = p[1]
    elif p[2] == '[':
        p[0] = ('array_access', p[1], p[3])
    elif p[2] == '(':
        p[0] = ('func_call', p[1], p[3])
    elif p[2] == '.':
        p[0] = ('struct_access', p[1], p[3])
    elif p[2] == '->':
        p[0] = ('ptr_access', p[1], p[3])

def p_argumentos(p):
    '''argumentos : expressao_lista
                  | vazio'''
    p[0] = p[1]

def p_primaria(p):
    '''primaria : ID
                | NUM_INT
                | NUM_DEC
                | TEXTO
                | LPAREN expressao RPAREN'''
    if len(p) == 2:
        p[0] = p[1]
    else:
        p[0] = p[2]

def p_vazio(p):
    '''vazio :'''
    p[0] = None

def p_error(p):
    print(f"Syntax error at '{p.value}'")

parser = yacc.yacc()

if __name__ == "__main__":
    import sys
    file_path = "C:\\Users\\santo\\Desktop\\ProjetoSemantico\\comp\\input.txt"
    with open(file_path, 'r') as file:
        data = file.read()
    result = parser.parse(data)
    print(result)

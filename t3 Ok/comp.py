import ply.lex as lex
import ply.yacc as yacc

# Lexer
tokens = [
    'ID', 'NUM_INT', 'NUM_DEC', 'STRING', 'EQUALS',
    'PLUS', 'MINUS', 'TIMES', 'DIV', 'MOD', 'AND', 'OR', 'NOT',
    'LT', 'GT', 'LE', 'GE', 'EQ', 'NE', 'LPAREN', 'RPAREN', 'LBRACE', 'RBRACE',
    'SEMICOLON', 'COMMA'
]

reserved = {
    'int': 'INT',
    'float': 'FLOAT',
    'double': 'DOUBLE',
    'char': 'CHAR',
    'boolean': 'BOOLEAN',
    'if': 'IF',
    'else': 'ELSE',
    'while': 'WHILE',
    'for': 'FOR',
    'return': 'RETURN',
    'println': 'PRINTLN'
}

tokens += list(reserved.values())

t_EQUALS = r'='
t_PLUS = r'\+'
t_MINUS = r'-'
t_TIMES = r'\*'
t_DIV = r'/'
t_MOD = r'%'
t_AND = r'&&'
t_OR = r'\|\|'
t_NOT = r'!'
t_LT = r'<'
t_GT = r'>'
t_LE = r'<='
t_GE = r'>='
t_EQ = r'=='
t_NE = r'!='
t_LPAREN = r'\('
t_RPAREN = r'\)'
t_LBRACE = r'\{'
t_RBRACE = r'\}'
t_SEMICOLON = r';'
t_COMMA = r','

def t_COMMENT(t):
    r'//.*'
    pass

def t_ID(t):
    r'[a-zA-Z_][a-zA-Z_0-9]*'
    t.type = reserved.get(t.value, 'ID')
    return t

def t_NUM_DEC(t):
    r'\d+\.\d+'
    t.value = float(t.value)
    return t

def t_NUM_INT(t):
    r'\d+'
    t.value = int(t.value)
    return t

def t_STRING(t):
    r'\"([^\\\n]|(\\.))*?\"'
    t.value = str(t.value)
    return t

t_ignore = ' \t'

def t_newline(t):
    r'\n+'
    t.lexer.lineno += len(t.value)

def t_error(t):
    print(f"Illegal character '{t.value[0]}'")
    t.lexer.skip(1)

lexer = lex.lex()

# Parser
precedence = (
    ('left', 'OR'),
    ('left', 'AND'),
    ('left', 'EQ', 'NE'),
    ('left', 'LT', 'LE', 'GT', 'GE'),
    ('left', 'PLUS', 'MINUS'),
    ('left', 'TIMES', 'DIV', 'MOD'),
    ('right', 'NOT'),
    ('right', 'UMINUS')
)

# Symbol Table
symbol_table = {}

def p_program(p):
    '''program : statement_list'''
    p[0] = ('program', p[1])

def p_statement_list(p):
    '''statement_list : statement_list statement
                      | statement'''
    if len(p) == 3:
        p[0] = p[1] + [p[2]]
    else:
        p[0] = [p[1]]

def p_statement(p):
    '''statement : var_declaration
                 | assignment
                 | function_declaration
                 | if_statement
                 | while_statement
                 | for_statement
                 | return_statement
                 | expression_statement
                 | print_statement
                 | compound_statement'''
    p[0] = p[1]

def p_var_declaration(p):
    '''var_declaration : type_specifier ID SEMICOLON
                       | type_specifier ID EQUALS expression SEMICOLON'''
    if len(p) == 4:
        p[0] = ('var_declaration', p[1], p[2])
        symbol_table[p[2]] = p[1]
    else:
        p[0] = ('var_declaration', p[1], p[2], p[4])
        symbol_table[p[2]] = p[1]
        check_type_compatibility(p[1], p[4])

def p_type_specifier(p):
    '''type_specifier : INT
                      | FLOAT
                      | DOUBLE
                      | CHAR
                      | BOOLEAN'''
    p[0] = p[1]

def p_assignment(p):
    '''assignment : ID EQUALS expression SEMICOLON'''
    p[0] = ('assignment', p[1], p[3])
    if p[1] in symbol_table:
        check_type_compatibility(symbol_table[p[1]], p[3])
    else:
        print(f"Undeclared variable '{p[1]}'")

def p_function_declaration(p):
    '''function_declaration : type_specifier ID LPAREN parameter_list RPAREN compound_statement'''
    p[0] = ('function_declaration', p[1], p[2], p[4], p[6])
    symbol_table[p[2]] = p[1]

def p_parameter_list(p):
    '''parameter_list : parameter_list COMMA parameter
                      | parameter
                      | empty'''
    if len(p) == 4:
        p[0] = p[1] + [p[3]]
    elif len(p) == 2:
        p[0] = [p[1]]
    else:
        p[0] = []

def p_parameter(p):
    '''parameter : type_specifier ID'''
    p[0] = (p[1], p[2])
    symbol_table[p[2]] = p[1]

def p_if_statement(p):
    '''if_statement : IF LPAREN expression RPAREN compound_statement
                    | IF LPAREN expression RPAREN compound_statement ELSE compound_statement'''
    if len(p) == 6:
        p[0] = ('if_statement', p[3], p[5])
    else:
        p[0] = ('if_else_statement', p[3], p[5], p[7])

def p_while_statement(p):
    '''while_statement : WHILE LPAREN expression RPAREN compound_statement'''
    p[0] = ('while_statement', p[3], p[5])

def p_for_statement(p):
    '''for_statement : FOR LPAREN expression SEMICOLON expression SEMICOLON expression RPAREN compound_statement'''
    p[0] = ('for_statement', p[3], p[5], p[7], p[9])

def p_return_statement(p):
    '''return_statement : RETURN expression SEMICOLON'''
    p[0] = ('return_statement', p[2])

def p_expression_statement(p):
    '''expression_statement : expression SEMICOLON'''
    p[0] = ('expression_statement', p[1])

def p_print_statement(p):
    '''print_statement : PRINTLN LPAREN STRING RPAREN SEMICOLON'''
    p[0] = ('print_statement', p[3])

def p_expression(p):
    '''expression : logical_or_expression'''
    p[0] = p[1]

def p_logical_or_expression(p):
    '''logical_or_expression : logical_or_expression OR logical_and_expression
                             | logical_and_expression'''
    if len(p) == 4:
        p[0] = ('logical_or', p[1], p[3])
        check_type_compatibility('boolean', p[1])
        check_type_compatibility('boolean', p[3])
    else:
        p[0] = p[1]

def p_logical_and_expression(p):
    '''logical_and_expression : logical_and_expression AND equality_expression
                              | equality_expression'''
    if len(p) == 4:
        p[0] = ('logical_and', p[1], p[3])
        check_type_compatibility('boolean', p[1])
        check_type_compatibility('boolean', p[3])
    else:
        p[0] = p[1]

def p_equality_expression(p):
    '''equality_expression : equality_expression EQ relational_expression
                           | equality_expression NE relational_expression
                           | relational_expression'''
    if len(p) == 4:
        p[0] = ('equality', p[1], p[2], p[3])
        check_type_compatibility(p[1], p[3])
    else:
        p[0] = p[1]

def p_relational_expression(p):
    '''relational_expression : relational_expression LT additive_expression
                             | relational_expression GT additive_expression
                             | relational_expression LE additive_expression
                             | relational_expression GE additive_expression
                             | additive_expression'''
    if len(p) == 4:
        p[0] = ('relational', p[1], p[2], p[3])
        check_type_compatibility(p[1], p[3])
    else:
        p[0] = p[1]

def p_additive_expression(p):
    '''additive_expression : additive_expression PLUS multiplicative_expression
                           | additive_expression MINUS multiplicative_expression
                           | multiplicative_expression'''
    if len(p) == 4:
        p[0] = ('additive', p[1], p[2], p[3])
        check_type_compatibility(p[1], p[3])
    else:
        p[0] = p[1]

def p_multiplicative_expression(p):
    '''multiplicative_expression : multiplicative_expression TIMES unary_expression
                                 | multiplicative_expression DIV unary_expression
                                 | multiplicative_expression MOD unary_expression
                                 | unary_expression'''
    if len(p) == 4:
        p[0] = ('multiplicative', p[1], p[2], p[3])
        check_type_compatibility(p[1], p[3])
    else:
        p[0] = p[1]

def p_unary_expression(p):
    '''unary_expression : MINUS unary_expression %prec UMINUS
                        | NOT unary_expression
                        | postfix_expression'''
    if len(p) == 3:
        p[0] = ('unary', p[1], p[2])
        if p[1] == '!':
            check_type_compatibility('boolean', p[2])
        else:
            check_type_compatibility('int', p[2])
    else:
        p[0] = p[1]

def p_postfix_expression(p):
    '''postfix_expression : primary_expression
                          | ID LPAREN argument_list RPAREN'''
    if len(p) == 2:
        p[0] = p[1]
    else:
        p[0] = ('function_call', p[1], p[3])

def p_argument_list(p):
    '''argument_list : argument_list COMMA expression
                     | expression
                     | empty'''
    if len(p) == 4:
        p[0] = p[1] + [p[3]]
    elif len(p) == 2:
        p[0] = [p[1]]
    else:
        p[0] = []

def p_primary_expression(p):
    '''primary_expression : ID
                          | NUM_INT
                          | NUM_DEC
                          | STRING
                          | LPAREN expression RPAREN'''
    if len(p) == 2:
        p[0] = p[1]
    else:
        p[0] = p[2]

def p_empty(p):
    '''empty :'''
    p[0] = None

def p_compound_statement(p):
    '''compound_statement : LBRACE statement_list RBRACE'''
    p[0] = ('compound_statement', p[2])

def p_error(p):
    print(f"Syntax error at '{p.value}'")

parser = yacc.yacc()

def check_type_compatibility(expected_type, actual_value):
    if isinstance(actual_value, tuple):
        actual_type = infer_type(actual_value)
    else:
        actual_type = type(actual_value).__name__

    if expected_type != actual_type:
        print(f"Type error: Expected {expected_type}, got {actual_type}")

def infer_type(expression):
    if isinstance(expression, tuple):
        if expression[0] in ('additive', 'multiplicative', 'unary'):
            return 'int'
        elif expression[0] in ('logical_or', 'logical_and', 'equality', 'relational'):
            return 'boolean'
    else:
        return type(expression).__name__

if __name__ == "__main__":
    import sys
    with open('input.txt', 'r') as file:
        data = file.read()
    result = parser.parse(data)
    print(result)


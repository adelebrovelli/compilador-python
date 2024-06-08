import ply.lex as lex
import ply.yacc as yacc

# Definição dos tokens
tokens = [
    'ID', 'NUM_INT', 'NUM_DEC', 'TEXTO', 'EQUALS', 'PLUS_EQUALS', 'MINUS_EQUALS',
    'TIMES_EQUALS', 'DIV_EQUALS', 'MOD_EQUALS', 'AND_EQUALS', 'OR_EQUALS',
    'PLUS', 'MINUS', 'TIMES', 'DIV', 'MOD', 'AND', 'OR', 'NOT',
    'LT', 'GT', 'LE', 'GE', 'EQ', 'NE', 'LPAREN', 'RPAREN', 'LBRACE', 'RBRACE',
    'SEMICOLON', 'COMMA', 'DOT', 'ARROW', 'INCREMENT', 'DECREMENT', 'COMMENT',
    'MULTI_COMMENT', 'LBRACKET', 'RBRACKET', 'COLON', 'DOTS'
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
    'switch': 'SWITCH',
    'case': 'CASE',
    'default': 'DEFAULT',
    'break': 'BREAK',
    'continue': 'CONTINUE',
    'return': 'RETURN',
    'struct': 'STRUCT',
    'void': 'VOID',
    'println': 'PRINTLN'
}

tokens = tokens + list(reserved.values())

t_EQUALS = r'='
t_PLUS_EQUALS = r'\+='
t_MINUS_EQUALS = r'-='
t_TIMES_EQUALS = r'\*='
t_DIV_EQUALS = r'/='
t_MOD_EQUALS = r'%='
t_AND_EQUALS = r'&&='
t_OR_EQUALS = r'\|\|='
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
t_DOT = r'\.'
t_ARROW = r'->'
t_INCREMENT = r'\+\+'
t_DECREMENT = r'--'
t_LBRACKET = r'\['
t_RBRACKET = r'\]'
t_COLON = r':'
t_DOTS = r'\.\.\.'

def t_COMMENT(t):
    r'//.*'
    pass  # Comentário de linha única, ignorar

def t_MULTI_COMMENT(t):
    r'/\[^]\+(?:[^/][^]\+)*/'
    pass  # Comentário de múltiplas linhas, ignorar

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

def t_TEXTO(t):
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

# Definição das regras de gramática
def p_programa(p):
    '''programa : declaracao_list'''
    p[0] = ('programa', p[1])

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

if "main" == "_main_":
    import sys
    with open(sys.argv[1], 'r') as file:
        data = file.read()
    result = parser.parse(data)
    print(result)
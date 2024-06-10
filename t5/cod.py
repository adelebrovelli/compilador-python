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

# Tokens
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
    r'/\*.*?\*/'
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
    report_error(f"Caractere ilegal '{t.value[0]}'", t.lineno)
    t.lexer.skip(1)

lexer = lex.lex()

# Ambiente de símbolos
symbol_table = {}

# Sobrecarga de operadores (exemplo básico)
operator_overload = {
    ('+', 'string', 'string'): 'string',
    ('+', 'int', 'int'): 'int',
    ('+', 'float', 'float'): 'float',
    ('+', 'int', 'float'): 'float',
    ('+', 'float', 'int'): 'float',
    # Adicione mais sobrecargas conforme necessário
}

# Função auxiliar para verificar tipos e aplicar coesão
def check_type(op, left, right):
    if (op, left, right) in operator_overload:
        return operator_overload[(op, left, right)]
    if op in ['+', '-', '*', '/', '%']:
        if left == right == 'int' or left == right == 'float':
            return left
        elif (left == 'int' and right == 'float') or (left == 'float' and right == 'int'):
            return 'float'
    elif op in ['<', '>', '<=', '>=', '==', '!=']:
        if left == right:
            return 'boolean'
    elif op in ['&&', '||']:
        if left == right == 'boolean':
            return 'boolean'
    elif op == '=':
        if left == right or (left == 'float' and right == 'int') or (left == 'int' and right == 'float'):
            return left
    return None

# Definição das regras de precedência dos operadores
precedence = (
    ('left', 'OR'),
    ('left', 'AND'),
    ('left', 'EQ', 'NE'),
    ('left', 'LT', 'LE', 'GT', 'GE'),
    ('left', 'PLUS', 'MINUS'),
    ('left', 'TIMES', 'DIV', 'MOD')
)

# Função de relatório de erro
error_list = []

def report_error(message, lineno):
    error_list.append(f"Linha {lineno}: {message}")

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
                  | estrutura_controle
                  | comentario
                  | expressao SEMICOLON'''
    p[0] = p[1]

def p_declaracao_variavel(p):
    '''declaracao_variavel : tipo ID SEMICOLON
                           | tipo ID EQUALS expressao SEMICOLON'''
    if len(p) == 4:
        symbol_table[p[2]] = p[1]
        p[0] = ('declaracao_variavel', p[1], p[2])
    else:
        expr_type = p[4][2] if len(p[4]) > 2 else 'desconhecido'
        if p[1] == expr_type or (p[1] == 'float' and expr_type == 'int') or (p[1] == 'int' and expr_type == 'float'):
            symbol_table[p[2]] = p[1]
            p[0] = ('declaracao_variavel', p[1], p[2], p[4])
        else:
            report_error(f"Erro de tipo: não é possível atribuir {expr_type} a {p[1]} para a variável {p[2]}", p.lineno(2))
            p[0] = ('error',)

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
    symbol_table[p[2]] = p[1]  # Adiciona o tipo de retorno da função à tabela de símbolos
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

def p_estrutura_controle(p):
    '''estrutura_controle : if
                          | while
                          | for
                          | switch'''
    p[0] = p[1]

def p_if(p):
    '''if : IF LPAREN expressao RPAREN bloco'''
    p[0] = ('if', p[3], p[5])

def p_while(p):
    '''while : WHILE LPAREN expressao RPAREN bloco'''
    p[0] = ('while', p[3], p[5])

def p_for(p):
    '''for : FOR LPAREN expressao SEMICOLON expressao SEMICOLON expressao RPAREN bloco'''
    p[0] = ('for', p[3], p[5], p[7], p[9])

def p_switch(p):
    '''switch : SWITCH LPAREN expressao RPAREN LBRACE case_list RBRACE'''
    p[0] = ('switch', p[3], p[6])

def p_case_list(p):
    '''case_list : case_list case
                 | case'''
    if len(p) == 3:
        p[0] = p[1] + [p[2]]
    else:
        p[0] = [p[1]]

def p_case(p):
    '''case : CASE expressao COLON declaracao_list
            | DEFAULT COLON declaracao_list'''
    if len(p) == 5:
        p[0] = ('case', p[2], p[4])
    else:
        p[0] = ('default', p[3])

def p_comentario(p):
    '''comentario : COMMENT
                  | MULTI_COMMENT'''
    p[0] = p[1]

def p_expressao(p):
    '''expressao : ID
                 | NUM_INT
                 | NUM_DEC
                 | TEXTO
                 | ID EQUALS expressao
                 | expressao PLUS expressao
                 | expressao MINUS expressao
                 | expressao TIMES expressao
                 | expressao DIV expressao
                 | expressao MOD expressao
                 | LPAREN expressao RPAREN
                 | PRINTLN LPAREN TEXTO RPAREN'''
    if len(p) == 2:
        if isinstance(p[1], int):
            p[0] = ('numero_inteiro', p[1], 'int')
        elif isinstance(p[1], float):
            p[0] = ('numero_decimal', p[1], 'float')
        elif isinstance(p[1], str) and p[1][0] == '"' and p[1][-1] == '"':
            p[0] = ('texto', p[1], 'string')
        else:
            p[0] = ('id', p[1], symbol_table.get(p[1], 'desconhecido'))
            if p[0][2] == 'desconhecido':
                report_error(f"Variável não declarada '{p[1]}'", p.lineno(1))
    elif len(p) == 4 and p[2] == '=':
        expr_type = p[3][2] if len(p[3]) > 2 else 'desconhecido'
        var_type = symbol_table.get(p[1], 'desconhecido')
        if var_type == 'desconhecido':
            report_error(f"Variável não declarada '{p[1]}'", p.lineno(1))
        elif var_type == expr_type or (var_type == 'float' and expr_type == 'int') or (var_type == 'int' and expr_type == 'float'):
            p[0] = ('atribuicao', p[1], p[3], expr_type)
        else:
            report_error(f"Erro de tipo: não é possível atribuir {expr_type} a {var_type} para a variável {p[1]}", p.lineno(1))
            p[0] = ('error',)
    elif len(p) == 4:
        left_type = p[1][2] if len(p[1]) > 2 else 'desconhecido'
        right_type = p[3][2] if len(p[3]) > 2 else 'desconhecido'
        result_type = check_type(p[2], left_type, right_type)
        if result_type:
            p[0] = ('operacao', p[1], p[2], p[3], result_type)
        else:
            report_error(f"Erro de tipo: tipos incompatíveis {left_type} e {right_type} para o operador {p[2]}", p.lineno(1))
            p[0] = ('error',)
    elif len(p) == 4 and p[1] == '(':
        p[0] = p[2]
    elif len(p) == 5:
        p[0] = ('println', p[3])
    else:
        report_error(f"Erro de sintaxe em {p.value}", p.lineno(1))

def p_vazio(p):
    '''vazio :'''
    p[0] = None

def p_error(p):
    if p:
        report_error(f"Erro de sintaxe em '{p.value}'", p.lineno)
    else:
        print("Erro de sintaxe no final do arquivo")

parser = yacc.yacc()

def infer_type(node):
    if isinstance(node, tuple):
        if node[0] == 'numero_inteiro':
            return 'int'
        elif node[0] == 'numero_decimal':
            return 'float'
        elif node[0] == 'texto':
            return 'string'
        elif node[0] == 'id':
            return symbol_table.get(node[1], 'desconhecido')
        elif node[0] == 'operacao':
            left_type = infer_type(node[1])
            right_type = infer_type(node[3])
            return check_type(node[2], left_type, right_type)
        elif node[0] == 'atribuicao':
            return infer_type(node[3])
    return 'desconhecido'

def semantic_analysis(ast):
    # Função para realizar a análise semântica na AST
    def check_node(node):
        if isinstance(node, tuple):
            if node[0] == 'atribuicao':
                var_name = node[1]
                var_type = symbol_table.get(var_name, 'desconhecido')
                expr_type = infer_type(node[3])
                if var_type == 'desconhecido':
                    report_error(f"Variável não declarada '{var_name}'", node[1])
                elif var_type != expr_type and not (var_type == 'float' and expr_type == 'int') and not (var_type == 'int' and expr_type == 'float'):
                    report_error(f"Erro de tipo: não é possível atribuir {expr_type} a {var_type} para a variável {var_name}", node[1])
            elif node[0] == 'declaracao_funcao':
                func_name = node[2]
                params = node[3]
                body = node[4]
                # Verificar tipos de retorno e parâmetros da função
                check_node(body)
            elif node[0] == 'operacao':
                left_type = infer_type(node[1])
                right_type = infer_type(node[3])
                op = node[2]
                if check_type(op, left_type, right_type) is None:
                    report_error(f"Tipos incompatíveis: não é possível usar {op} com {left_type} e {right_type}", node[1])
            for child in node[1:]:
                check_node(child)

    check_node(ast)
    return error_list

def test_parser(file_path):
    with open(file_path, 'r') as file:
        data = file.read()
    result = parser.parse(data)
    if result:
        errors = semantic_analysis(result)
        print("Relatório de Análise Semântica:")
        print("=========================")
        if errors:
            for i, error in enumerate(errors):
                print(f"{i+1}. Erro: {error}")
        print("=========================")
        print(f"Análise concluída com {len(errors)} erros.")
    else:
        print("Erros de sintaxe detectados. Análise semântica abortada.")

# Código para ler o arquivo input.txt
file_path = 'input3.txt'
test_parser(file_path)

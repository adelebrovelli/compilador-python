
# parsetab.py
# This file is automatically generated. Do not edit.
# pylint: disable=W,C,R
_tabversion = '3.10'

_lr_method = 'LALR'

_lr_signature = 'leftORleftANDleftEQNEleftLTLEGTGEleftPLUSMINUSleftTIMESDIVMODAND AND_EQUALS ARROW BOOLEAN BREAK CASE CHAR COLON COMMA COMMENT CONTINUE DECREMENT DEFAULT DIV DIV_EQUALS DOT DOTS DOUBLE ELSE EQ EQUALS FLOAT FOR GE GT ID IF INCREMENT INT LBRACE LBRACKET LE LPAREN LT MINUS MINUS_EQUALS MOD MOD_EQUALS MULTI_COMMENT NE NOT NUM_DEC NUM_INT OR OR_EQUALS PLUS PLUS_EQUALS PRINTLN RBRACE RBRACKET RETURN RPAREN SEMICOLON STRUCT SWITCH TEXTO TIMES TIMES_EQUALS VOID WHILEprograma : declaracao_listdeclaracao_list : declaracao_list declaracao\n                       | declaracaodeclaracao : declaracao_variavel\n                  | declaracao_funcao\n                  | estrutura_controle\n                  | comentario\n                  | expressao SEMICOLONdeclaracao_variavel : tipo ID SEMICOLON\n                           | tipo ID EQUALS expressao SEMICOLONtipo : INT\n            | FLOAT\n            | DOUBLE\n            | CHAR\n            | BOOLEAN\n            | VOIDdeclaracao_funcao : tipo ID LPAREN parametros RPAREN blocoparametros : parametro\n                  | parametro COMMA parametros\n                  | vazioparametro : tipo ID\n                 | tipo ID LBRACKET RBRACKET\n                 | tipo DOTS IDbloco : LBRACE declaracao_list RBRACEestrutura_controle : if\n                          | while\n                          | for\n                          | switchif : IF LPAREN expressao RPAREN blocowhile : WHILE LPAREN expressao RPAREN blocofor : FOR LPAREN expressao SEMICOLON expressao SEMICOLON expressao RPAREN blocoswitch : SWITCH LPAREN expressao RPAREN LBRACE case_list RBRACEcase_list : case_list case\n                 | casecase : CASE expressao COLON declaracao_list\n            | DEFAULT COLON declaracao_listcomentario : COMMENT\n                  | MULTI_COMMENTexpressao : ID\n                 | NUM_INT\n                 | NUM_DEC\n                 | TEXTO\n                 | ID EQUALS expressao\n                 | expressao PLUS expressao\n                 | expressao MINUS expressao\n                 | expressao TIMES expressao\n                 | expressao DIV expressao\n                 | expressao MOD expressao\n                 | LPAREN expressao RPAREN\n                 | PRINTLN LPAREN TEXTO RPARENvazio :'
    
_lr_action_items = {'COMMENT':([0,2,3,4,5,6,7,12,13,14,15,16,17,32,33,52,72,77,78,79,84,86,93,95,98,100,101,102,103,],[16,16,-3,-4,-5,-6,-7,-25,-26,-27,-28,-37,-38,-2,-8,-9,-10,-29,16,-30,-17,16,-24,-32,16,16,16,-31,16,]),'MULTI_COMMENT':([0,2,3,4,5,6,7,12,13,14,15,16,17,32,33,52,72,77,78,79,84,86,93,95,98,100,101,102,103,],[17,17,-3,-4,-5,-6,-7,-25,-26,-27,-28,-37,-38,-2,-8,-9,-10,-29,17,-30,-17,17,-24,-32,17,17,17,-31,17,]),'ID':([0,2,3,4,5,6,7,9,11,12,13,14,15,16,17,22,23,24,25,26,27,32,33,34,35,36,37,38,40,43,44,45,46,52,53,63,70,72,74,77,78,79,84,86,87,90,93,95,98,100,101,102,103,],[10,10,-3,-4,-5,-6,-7,39,10,-25,-26,-27,-28,-37,-38,-11,-12,-13,-14,-15,-16,-2,-8,10,10,10,10,10,10,10,10,10,10,-9,10,73,10,-10,83,-29,10,-30,-17,10,10,10,-24,-32,10,10,10,-31,10,]),'NUM_INT':([0,2,3,4,5,6,7,11,12,13,14,15,16,17,32,33,34,35,36,37,38,40,43,44,45,46,52,53,70,72,77,78,79,84,86,87,90,93,95,98,100,101,102,103,],[18,18,-3,-4,-5,-6,-7,18,-25,-26,-27,-28,-37,-38,-2,-8,18,18,18,18,18,18,18,18,18,18,-9,18,18,-10,-29,18,-30,-17,18,18,18,-24,-32,18,18,18,-31,18,]),'NUM_DEC':([0,2,3,4,5,6,7,11,12,13,14,15,16,17,32,33,34,35,36,37,38,40,43,44,45,46,52,53,70,72,77,78,79,84,86,87,90,93,95,98,100,101,102,103,],[19,19,-3,-4,-5,-6,-7,19,-25,-26,-27,-28,-37,-38,-2,-8,19,19,19,19,19,19,19,19,19,19,-9,19,19,-10,-29,19,-30,-17,19,19,19,-24,-32,19,19,19,-31,19,]),'TEXTO':([0,2,3,4,5,6,7,11,12,13,14,15,16,17,32,33,34,35,36,37,38,40,42,43,44,45,46,52,53,70,72,77,78,79,84,86,87,90,93,95,98,100,101,102,103,],[20,20,-3,-4,-5,-6,-7,20,-25,-26,-27,-28,-37,-38,-2,-8,20,20,20,20,20,20,57,20,20,20,20,-9,20,20,-10,-29,20,-30,-17,20,20,20,-24,-32,20,20,20,-31,20,]),'LPAREN':([0,2,3,4,5,6,7,11,12,13,14,15,16,17,21,28,29,30,31,32,33,34,35,36,37,38,39,40,43,44,45,46,52,53,70,72,77,78,79,84,86,87,90,93,95,98,100,101,102,103,],[11,11,-3,-4,-5,-6,-7,11,-25,-26,-27,-28,-37,-38,42,43,44,45,46,-2,-8,11,11,11,11,11,54,11,11,11,11,11,-9,11,11,-10,-29,11,-30,-17,11,11,11,-24,-32,11,11,11,-31,11,]),'PRINTLN':([0,2,3,4,5,6,7,11,12,13,14,15,16,17,32,33,34,35,36,37,38,40,43,44,45,46,52,53,70,72,77,78,79,84,86,87,90,93,95,98,100,101,102,103,],[21,21,-3,-4,-5,-6,-7,21,-25,-26,-27,-28,-37,-38,-2,-8,21,21,21,21,21,21,21,21,21,21,-9,21,21,-10,-29,21,-30,-17,21,21,21,-24,-32,21,21,21,-31,21,]),'INT':([0,2,3,4,5,6,7,12,13,14,15,16,17,32,33,52,54,72,76,77,78,79,84,86,93,95,98,100,101,102,103,],[22,22,-3,-4,-5,-6,-7,-25,-26,-27,-28,-37,-38,-2,-8,-9,22,-10,22,-29,22,-30,-17,22,-24,-32,22,22,22,-31,22,]),'FLOAT':([0,2,3,4,5,6,7,12,13,14,15,16,17,32,33,52,54,72,76,77,78,79,84,86,93,95,98,100,101,102,103,],[23,23,-3,-4,-5,-6,-7,-25,-26,-27,-28,-37,-38,-2,-8,-9,23,-10,23,-29,23,-30,-17,23,-24,-32,23,23,23,-31,23,]),'DOUBLE':([0,2,3,4,5,6,7,12,13,14,15,16,17,32,33,52,54,72,76,77,78,79,84,86,93,95,98,100,101,102,103,],[24,24,-3,-4,-5,-6,-7,-25,-26,-27,-28,-37,-38,-2,-8,-9,24,-10,24,-29,24,-30,-17,24,-24,-32,24,24,24,-31,24,]),'CHAR':([0,2,3,4,5,6,7,12,13,14,15,16,17,32,33,52,54,72,76,77,78,79,84,86,93,95,98,100,101,102,103,],[25,25,-3,-4,-5,-6,-7,-25,-26,-27,-28,-37,-38,-2,-8,-9,25,-10,25,-29,25,-30,-17,25,-24,-32,25,25,25,-31,25,]),'BOOLEAN':([0,2,3,4,5,6,7,12,13,14,15,16,17,32,33,52,54,72,76,77,78,79,84,86,93,95,98,100,101,102,103,],[26,26,-3,-4,-5,-6,-7,-25,-26,-27,-28,-37,-38,-2,-8,-9,26,-10,26,-29,26,-30,-17,26,-24,-32,26,26,26,-31,26,]),'VOID':([0,2,3,4,5,6,7,12,13,14,15,16,17,32,33,52,54,72,76,77,78,79,84,86,93,95,98,100,101,102,103,],[27,27,-3,-4,-5,-6,-7,-25,-26,-27,-28,-37,-38,-2,-8,-9,27,-10,27,-29,27,-30,-17,27,-24,-32,27,27,27,-31,27,]),'IF':([0,2,3,4,5,6,7,12,13,14,15,16,17,32,33,52,72,77,78,79,84,86,93,95,98,100,101,102,103,],[28,28,-3,-4,-5,-6,-7,-25,-26,-27,-28,-37,-38,-2,-8,-9,-10,-29,28,-30,-17,28,-24,-32,28,28,28,-31,28,]),'WHILE':([0,2,3,4,5,6,7,12,13,14,15,16,17,32,33,52,72,77,78,79,84,86,93,95,98,100,101,102,103,],[29,29,-3,-4,-5,-6,-7,-25,-26,-27,-28,-37,-38,-2,-8,-9,-10,-29,29,-30,-17,29,-24,-32,29,29,29,-31,29,]),'FOR':([0,2,3,4,5,6,7,12,13,14,15,16,17,32,33,52,72,77,78,79,84,86,93,95,98,100,101,102,103,],[30,30,-3,-4,-5,-6,-7,-25,-26,-27,-28,-37,-38,-2,-8,-9,-10,-29,30,-30,-17,30,-24,-32,30,30,30,-31,30,]),'SWITCH':([0,2,3,4,5,6,7,12,13,14,15,16,17,32,33,52,72,77,78,79,84,86,93,95,98,100,101,102,103,],[31,31,-3,-4,-5,-6,-7,-25,-26,-27,-28,-37,-38,-2,-8,-9,-10,-29,31,-30,-17,31,-24,-32,31,31,31,-31,31,]),'$end':([1,2,3,4,5,6,7,12,13,14,15,16,17,32,33,52,72,77,79,84,93,95,102,],[0,-1,-3,-4,-5,-6,-7,-25,-26,-27,-28,-37,-38,-2,-8,-9,-10,-29,-30,-17,-24,-32,-31,]),'RBRACE':([3,4,5,6,7,12,13,14,15,16,17,32,33,52,72,77,79,84,86,88,89,93,95,96,101,102,103,],[-3,-4,-5,-6,-7,-25,-26,-27,-28,-37,-38,-2,-8,-9,-10,-29,-30,-17,93,95,-34,-24,-32,-33,-36,-31,-35,]),'CASE':([3,4,5,6,7,12,13,14,15,16,17,32,33,52,72,77,79,81,84,88,89,93,95,96,101,102,103,],[-3,-4,-5,-6,-7,-25,-26,-27,-28,-37,-38,-2,-8,-9,-10,-29,-30,90,-17,90,-34,-24,-32,-33,-36,-31,-35,]),'DEFAULT':([3,4,5,6,7,12,13,14,15,16,17,32,33,52,72,77,79,81,84,88,89,93,95,96,101,102,103,],[-3,-4,-5,-6,-7,-25,-26,-27,-28,-37,-38,-2,-8,-9,-10,-29,-30,91,-17,91,-34,-24,-32,-33,-36,-31,-35,]),'SEMICOLON':([8,10,18,19,20,39,47,48,49,50,51,55,56,60,62,67,80,],[33,-39,-40,-41,-42,52,-44,-45,-46,-47,-48,-43,-49,70,72,-50,87,]),'PLUS':([8,10,18,19,20,41,47,48,49,50,51,55,56,58,59,60,61,62,67,80,94,97,],[34,-39,-40,-41,-42,34,-44,-45,-46,-47,-48,34,-49,34,34,34,34,34,-50,34,34,34,]),'MINUS':([8,10,18,19,20,41,47,48,49,50,51,55,56,58,59,60,61,62,67,80,94,97,],[35,-39,-40,-41,-42,35,-44,-45,-46,-47,-48,35,-49,35,35,35,35,35,-50,35,35,35,]),'TIMES':([8,10,18,19,20,41,47,48,49,50,51,55,56,58,59,60,61,62,67,80,94,97,],[36,-39,-40,-41,-42,36,36,36,-46,-47,-48,36,-49,36,36,36,36,36,-50,36,36,36,]),'DIV':([8,10,18,19,20,41,47,48,49,50,51,55,56,58,59,60,61,62,67,80,94,97,],[37,-39,-40,-41,-42,37,37,37,-46,-47,-48,37,-49,37,37,37,37,37,-50,37,37,37,]),'MOD':([8,10,18,19,20,41,47,48,49,50,51,55,56,58,59,60,61,62,67,80,94,97,],[38,-39,-40,-41,-42,38,38,38,-46,-47,-48,38,-49,38,38,38,38,38,-50,38,38,38,]),'RPAREN':([10,18,19,20,41,47,48,49,50,51,54,55,56,57,58,59,61,64,65,66,67,73,76,83,85,92,94,],[-39,-40,-41,-42,56,-44,-45,-46,-47,-48,-51,-43,-49,67,68,69,71,75,-18,-20,-50,-21,-51,-23,-19,-22,99,]),'COLON':([10,18,19,20,47,48,49,50,51,55,56,67,91,97,],[-39,-40,-41,-42,-44,-45,-46,-47,-48,-43,-49,-50,98,100,]),'EQUALS':([10,39,],[40,53,]),'DOTS':([22,23,24,25,26,27,63,],[-11,-12,-13,-14,-15,-16,74,]),'COMMA':([65,73,83,92,],[76,-21,-23,-22,]),'LBRACE':([68,69,71,75,99,],[78,78,81,78,78,]),'LBRACKET':([73,],[82,]),'RBRACKET':([82,],[92,]),}

_lr_action = {}
for _k, _v in _lr_action_items.items():
   for _x,_y in zip(_v[0],_v[1]):
      if not _x in _lr_action:  _lr_action[_x] = {}
      _lr_action[_x][_k] = _y
del _lr_action_items

_lr_goto_items = {'programa':([0,],[1,]),'declaracao_list':([0,78,98,100,],[2,86,101,103,]),'declaracao':([0,2,78,86,98,100,101,103,],[3,32,3,32,3,3,32,32,]),'declaracao_variavel':([0,2,78,86,98,100,101,103,],[4,4,4,4,4,4,4,4,]),'declaracao_funcao':([0,2,78,86,98,100,101,103,],[5,5,5,5,5,5,5,5,]),'estrutura_controle':([0,2,78,86,98,100,101,103,],[6,6,6,6,6,6,6,6,]),'comentario':([0,2,78,86,98,100,101,103,],[7,7,7,7,7,7,7,7,]),'expressao':([0,2,11,34,35,36,37,38,40,43,44,45,46,53,70,78,86,87,90,98,100,101,103,],[8,8,41,47,48,49,50,51,55,58,59,60,61,62,80,8,8,94,97,8,8,8,8,]),'tipo':([0,2,54,76,78,86,98,100,101,103,],[9,9,63,63,9,9,9,9,9,9,]),'if':([0,2,78,86,98,100,101,103,],[12,12,12,12,12,12,12,12,]),'while':([0,2,78,86,98,100,101,103,],[13,13,13,13,13,13,13,13,]),'for':([0,2,78,86,98,100,101,103,],[14,14,14,14,14,14,14,14,]),'switch':([0,2,78,86,98,100,101,103,],[15,15,15,15,15,15,15,15,]),'parametros':([54,76,],[64,85,]),'parametro':([54,76,],[65,65,]),'vazio':([54,76,],[66,66,]),'bloco':([68,69,75,99,],[77,79,84,102,]),'case_list':([81,],[88,]),'case':([81,88,],[89,96,]),}

_lr_goto = {}
for _k, _v in _lr_goto_items.items():
   for _x, _y in zip(_v[0], _v[1]):
       if not _x in _lr_goto: _lr_goto[_x] = {}
       _lr_goto[_x][_k] = _y
del _lr_goto_items
_lr_productions = [
  ("S' -> programa","S'",1,None,None,None),
  ('programa -> declaracao_list','programa',1,'p_programa','cod.py',166),
  ('declaracao_list -> declaracao_list declaracao','declaracao_list',2,'p_declaracao_list','cod.py',170),
  ('declaracao_list -> declaracao','declaracao_list',1,'p_declaracao_list','cod.py',171),
  ('declaracao -> declaracao_variavel','declaracao',1,'p_declaracao','cod.py',178),
  ('declaracao -> declaracao_funcao','declaracao',1,'p_declaracao','cod.py',179),
  ('declaracao -> estrutura_controle','declaracao',1,'p_declaracao','cod.py',180),
  ('declaracao -> comentario','declaracao',1,'p_declaracao','cod.py',181),
  ('declaracao -> expressao SEMICOLON','declaracao',2,'p_declaracao','cod.py',182),
  ('declaracao_variavel -> tipo ID SEMICOLON','declaracao_variavel',3,'p_declaracao_variavel','cod.py',186),
  ('declaracao_variavel -> tipo ID EQUALS expressao SEMICOLON','declaracao_variavel',5,'p_declaracao_variavel','cod.py',187),
  ('tipo -> INT','tipo',1,'p_tipo','cod.py',201),
  ('tipo -> FLOAT','tipo',1,'p_tipo','cod.py',202),
  ('tipo -> DOUBLE','tipo',1,'p_tipo','cod.py',203),
  ('tipo -> CHAR','tipo',1,'p_tipo','cod.py',204),
  ('tipo -> BOOLEAN','tipo',1,'p_tipo','cod.py',205),
  ('tipo -> VOID','tipo',1,'p_tipo','cod.py',206),
  ('declaracao_funcao -> tipo ID LPAREN parametros RPAREN bloco','declaracao_funcao',6,'p_declaracao_funcao','cod.py',210),
  ('parametros -> parametro','parametros',1,'p_parametros','cod.py',215),
  ('parametros -> parametro COMMA parametros','parametros',3,'p_parametros','cod.py',216),
  ('parametros -> vazio','parametros',1,'p_parametros','cod.py',217),
  ('parametro -> tipo ID','parametro',2,'p_parametro','cod.py',226),
  ('parametro -> tipo ID LBRACKET RBRACKET','parametro',4,'p_parametro','cod.py',227),
  ('parametro -> tipo DOTS ID','parametro',3,'p_parametro','cod.py',228),
  ('bloco -> LBRACE declaracao_list RBRACE','bloco',3,'p_bloco','cod.py',237),
  ('estrutura_controle -> if','estrutura_controle',1,'p_estrutura_controle','cod.py',241),
  ('estrutura_controle -> while','estrutura_controle',1,'p_estrutura_controle','cod.py',242),
  ('estrutura_controle -> for','estrutura_controle',1,'p_estrutura_controle','cod.py',243),
  ('estrutura_controle -> switch','estrutura_controle',1,'p_estrutura_controle','cod.py',244),
  ('if -> IF LPAREN expressao RPAREN bloco','if',5,'p_if','cod.py',248),
  ('while -> WHILE LPAREN expressao RPAREN bloco','while',5,'p_while','cod.py',252),
  ('for -> FOR LPAREN expressao SEMICOLON expressao SEMICOLON expressao RPAREN bloco','for',9,'p_for','cod.py',256),
  ('switch -> SWITCH LPAREN expressao RPAREN LBRACE case_list RBRACE','switch',7,'p_switch','cod.py',260),
  ('case_list -> case_list case','case_list',2,'p_case_list','cod.py',264),
  ('case_list -> case','case_list',1,'p_case_list','cod.py',265),
  ('case -> CASE expressao COLON declaracao_list','case',4,'p_case','cod.py',272),
  ('case -> DEFAULT COLON declaracao_list','case',3,'p_case','cod.py',273),
  ('comentario -> COMMENT','comentario',1,'p_comentario','cod.py',280),
  ('comentario -> MULTI_COMMENT','comentario',1,'p_comentario','cod.py',281),
  ('expressao -> ID','expressao',1,'p_expressao','cod.py',285),
  ('expressao -> NUM_INT','expressao',1,'p_expressao','cod.py',286),
  ('expressao -> NUM_DEC','expressao',1,'p_expressao','cod.py',287),
  ('expressao -> TEXTO','expressao',1,'p_expressao','cod.py',288),
  ('expressao -> ID EQUALS expressao','expressao',3,'p_expressao','cod.py',289),
  ('expressao -> expressao PLUS expressao','expressao',3,'p_expressao','cod.py',290),
  ('expressao -> expressao MINUS expressao','expressao',3,'p_expressao','cod.py',291),
  ('expressao -> expressao TIMES expressao','expressao',3,'p_expressao','cod.py',292),
  ('expressao -> expressao DIV expressao','expressao',3,'p_expressao','cod.py',293),
  ('expressao -> expressao MOD expressao','expressao',3,'p_expressao','cod.py',294),
  ('expressao -> LPAREN expressao RPAREN','expressao',3,'p_expressao','cod.py',295),
  ('expressao -> PRINTLN LPAREN TEXTO RPAREN','expressao',4,'p_expressao','cod.py',296),
  ('vazio -> <empty>','vazio',0,'p_vazio','cod.py',335),
]

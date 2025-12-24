from dataclasses import dataclass
from typing import List, Set


# ==========================================
# 1. 词法分析（DFA）- 已修复减法死循环
# ==========================================

@dataclass
class Token:
    type: str
    value: str
    line: int

    def __repr__(self):
        return f"<{self.type:10}, {self.value:10}, Line: {self.line}>"


def char_type(ch):
    if ch.isalpha() or ch == '_': return 'LETTER'
    if ch.isdigit(): return 'DIGIT'
    if ch == '.': return 'DOT'
    if ch.isspace(): return 'SPACE'
    return ch


DFA = {
    0: {
        'LETTER': 1, 'DIGIT': 2,
        '+': 3, '-': 'MINUS', '*': 'MUL', '/': 'DIV',  # 修复：直接定义减乘除为终结态
        '=': 5, '<': 6, '>': 6, '!': 6,
        ';': 'SEMI', '(': 'LPAREN', ')': 'RPAREN',
        '{': 'LBRACE', '}': 'RBRACE',
        'SPACE': 0
    },
    1: {'LETTER': 1, 'DIGIT': 1},  # ID
    2: {'DIGIT': 2, 'DOT': 7, 'LETTER': 99},  # NUM
    7: {'DIGIT': 8},  # FLOAT .
    8: {'DIGIT': 8},  # FLOAT
    3: {'+': 4},  # ++
    5: {'=': 6},
    6: {'=': 6},  # relop
    99: {'LETTER': 99, 'DIGIT': 99}  # error
}


class DFALexer:
    KEYWORDS = {'int': 'INT', 'float': 'FLOAT', 'for': 'FOR'}

    def __init__(self, code):
        self.code = code
        self.pos = 0
        self.line = 1
        self.tokens = []

    def tokenize(self):
        while self.pos < len(self.code):
            # 跳过空白符
            while self.pos < len(self.code) and self.code[self.pos].isspace():
                if self.code[self.pos] == '\n': self.line += 1
                self.pos += 1
            if self.pos >= len(self.code): break

            state = 0
            lexeme = ''

            while self.pos < len(self.code):
                ch = self.code[self.pos]
                ctype = char_type(ch)

                if state not in DFA or ctype not in DFA[state]:
                    break

                nxt = DFA[state][ctype]

                # 处理单字符终结符（如减号、乘号）
                if isinstance(nxt, str):
                    self.tokens.append(Token(nxt, ch, self.line))
                    self.pos += 1
                    state = -1  # 标记已处理
                    break

                state = nxt
                lexeme += ch
                self.pos += 1

            if state == -1: continue

            # 处理多字符 Token
            if state == 1:
                self.tokens.append(Token(self.KEYWORDS.get(lexeme, 'ID'), lexeme, self.line))
            elif state == 2:
                self.tokens.append(Token('NUM', lexeme, self.line))
            elif state == 8:
                self.tokens.append(Token('FLOAT_NUM', lexeme, self.line))
            elif state in [3, 4]:
                self.tokens.append(Token('++' if state == 4 else '+', lexeme, self.line))
            elif state == 5:
                self.tokens.append(Token('=', '=', self.line))
            elif state == 6:
                self.tokens.append(Token('relop', lexeme, self.line))
            else:
                # 错误处理：防止非法字符导致死循环
                if self.pos < len(self.code):
                    self.pos += 1

        self.tokens.append(Token('#', '#', self.line))
        return self.tokens


# ==========================================
# 2. 中间代码生成与符号表
# ==========================================

class TACGenerator:
    def __init__(self):
        self.code = []
        self.temp_count = 0
        self.label_count = 0

    def new_temp(self):
        self.temp_count += 1
        return f"t{self.temp_count}"

    def new_label(self):
        self.label_count += 1
        return f"L{self.label_count}"

    def emit(self, line):
        self.code.append(line)


# ==========================================
# 3. 语法分析（LL(1) + 四则运算）
# ==========================================

class LL1Parser:
    def __init__(self, tokens):
        self.tokens = tokens
        self.pos = 0
        self.stack = ['#', 'Program']
        self.data_stack = []
        self.tac = TACGenerator()
        self.sym_table: Set[str] = set()
        self.for_labels = []
        self.last_lexeme = ""
        self.nonterminals = {
            'Program', 'ForStmt', 'Init', 'Cond', 'Iter', 'IterSuffix',
            'Block', 'StmtList', 'Stmt', 'DeclStmt', 'DeclSuffix',
            'AssignStmt', 'Expr', "Expr'", 'Term', "Term'", 'Factor'
        }
        self.table = self.build_table()

    def peek(self):
        return self.tokens[self.pos]

    def advance(self):
        self.pos += 1

    def build_table(self):
        T = {}

        def add(A, a, prod):
            T[(A, a)] = prod

        # 核心文法定义
        add('Program', 'INT', ['StmtList']);
        add('Program', 'ID', ['StmtList']);
        add('Program', 'FOR', ['StmtList']);
        add('Program', '#', [])
        add('StmtList', 'INT', ['Stmt', 'StmtList']);
        add('StmtList', 'ID', ['Stmt', 'StmtList']);
        add('StmtList', 'FOR', ['Stmt', 'StmtList']);
        add('StmtList', 'RBRACE', []);
        add('StmtList', '#', [])
        add('Stmt', 'INT', ['DeclStmt']);
        add('Stmt', 'ID', ['AssignStmt']);
        add('Stmt', 'FOR', ['ForStmt'])

        add('DeclStmt', 'INT', ['INT', 'ID', '@DEF_VAR', 'DeclSuffix'])
        add('DeclSuffix', 'SEMI', ['SEMI'])
        add('DeclSuffix', '=', ['=', 'Expr', '@ASSIGN', 'SEMI'])

        add('AssignStmt', 'ID', ['ID', '@CHECK_VAR', '@PUSH_VAL', '=', 'Expr', '@ASSIGN', 'SEMI'])

        add('ForStmt', 'FOR',
            ['FOR', 'LPAREN', 'Init', 'SEMI', '@FOR_L1', 'Cond', '@FOR_IF', 'SEMI', 'Iter', 'RPAREN', 'Block',
             '@FOR_GOTO_L1'])

        add('Init', 'INT', ['INT', 'ID', '@DEF_VAR', '=', 'Expr', '@ASSIGN'])
        add('Init', 'ID', ['ID', '@CHECK_VAR', '@PUSH_VAL', '=', 'Expr', '@ASSIGN'])

        add('Cond', 'ID', ['Expr', 'relop', '@PUSH_OP', 'Expr', '@REL_GEN'])
        add('Cond', 'NUM', ['Expr', 'relop', '@PUSH_OP', 'Expr', '@REL_GEN'])

        add('Iter', 'ID', ['ID', '@CHECK_VAR', '@PUSH_VAL', 'IterSuffix'])
        add('Iter', '++', ['++', 'ID', '@CHECK_VAR', '@PUSH_VAL', '@INC'])
        add('IterSuffix', '=', ['=', 'Expr', '@ASSIGN'])
        add('IterSuffix', '++', ['++', '@INC'])

        add('Block', 'LBRACE', ['LBRACE', 'StmtList', 'RBRACE'])

        # --- 四则运算文法支持 ---
        add('Expr', 'ID', ['Term', "Expr'"]);
        add('Expr', 'NUM', ['Term', "Expr'"]);
        add('Expr', 'LPAREN', ['Term', "Expr'"])
        add("Expr'", '+', ['+', 'Term', '@ADD', "Expr'"])
        add("Expr'", 'MINUS', ['MINUS', 'Term', '@SUB', "Expr'"])  # 减法
        for t in ['SEMI', 'RPAREN', 'relop']: add("Expr'", t, [])

        add('Term', 'ID', ['Factor', "Term'"]);
        add('Term', 'NUM', ['Factor', "Term'"]);
        add('Term', 'LPAREN', ['Factor', "Term'"])
        add("Term'", 'MUL', ['MUL', 'Factor', '@MUL', "Term'"])  # 乘法
        add("Term'", 'DIV', ['DIV', 'Factor', '@DIV', "Term'"])  # 除法
        for t in ['+', 'MINUS', 'SEMI', 'RPAREN', 'relop']: add("Term'", t, [])

        add('Factor', 'ID', ['ID', '@CHECK_VAR', '@PUSH_VAL'])
        add('Factor', 'NUM', ['NUM', '@PUSH_VAL'])
        add('Factor', 'LPAREN', ['LPAREN', 'Expr', 'RPAREN'])

        return T

    def parse(self):
        while self.stack:
            top = self.stack.pop()
            cur = self.peek()
            if top.startswith('@'):
                self.execute_action(top)
                continue
            if top == '#':
                if cur.type == '#': return
                raise Exception("语法分析错误")
            if top not in self.nonterminals:
                if top == cur.type:
                    self.last_lexeme = cur.value
                    self.advance()
                else:
                    raise Exception(f"Line {cur.line}: 期望 {top}, 得到 {cur.type}")
            else:
                key = (top, cur.type)
                if key not in self.table:
                    raise Exception(f"Line {cur.line}: 无法在 {top} 下处理符号 {cur.type}")
                for sym in reversed(self.table[key]):
                    self.stack.append(sym)

    def execute_action(self, action):
        if action == '@DEF_VAR':
            if self.last_lexeme in self.sym_table:
                raise RuntimeError(f"语义错误: 变量 '{self.last_lexeme}' 重复定义")
            self.sym_table.add(self.last_lexeme)
            self.data_stack.append(self.last_lexeme)
        elif action == '@CHECK_VAR':
            if self.last_lexeme not in self.sym_table:
                raise RuntimeError(f"语义错误: 变量 '{self.last_lexeme}' 未定义")
        elif action in ['@PUSH_VAL', '@PUSH_OP']:
            self.data_stack.append(self.last_lexeme)
        elif action == '@ASSIGN':
            val = self.data_stack.pop();
            target = self.data_stack.pop()
            self.tac.emit(f"{target} = {val}")
        elif action in ['@ADD', '@SUB', '@MUL', '@DIV']:
            op = {'@ADD': '+', '@SUB': '-', '@MUL': '*', '@DIV': '/'}[action]
            r = self.data_stack.pop();
            l = self.data_stack.pop()
            t = self.tac.new_temp()
            self.tac.emit(f"{t} = {l} {op} {r}")
            self.data_stack.append(t)
        elif action == '@FOR_L1':
            L1 = self.tac.new_label()
            self.tac.emit(f"{L1}:")
            self.for_labels.append(L1)
        elif action == '@REL_GEN':
            r = self.data_stack.pop();
            op = self.data_stack.pop();
            l = self.data_stack.pop()
            t = self.tac.new_temp()
            self.tac.emit(f"{t} = {l} {op} {r}")
            self.data_stack.append(t)
        elif action == '@FOR_IF':
            cond = self.data_stack.pop()
            L2 = self.tac.new_label()
            self.tac.emit(f"if {cond} == 0 goto {L2}")
            self.for_labels.append(L2)
        elif action == '@INC':
            target = self.data_stack.pop()
            self.tac.emit(f"{target} = {target} + 1")
        elif action == '@FOR_GOTO_L1':
            L2 = self.for_labels.pop();
            L1 = self.for_labels.pop()
            self.tac.emit(f"goto {L1}\n{L2}:")


# ==========================================
# 4. 运行
# ==========================================

if __name__ == "__main__":
    # 测试代码：包含减法、乘法以及重复定义检测
    code = """
    for (int i = 0; i < 5; i++) {
        int a = 10 - 2 * 3;
        int b = a / 2;
    }
    """

    print("--- 1. 词法分析结果 ---")
    lexer = DFALexer(code)
    tokens = lexer.tokenize()
    for t in tokens: print(t)

    print("\n--- 2. 语法分析与三地址码 ---")
    try:
        parser = LL1Parser(tokens)
        parser.parse()
        for line in parser.tac.code: print(line)
    except Exception as e:
        print(f"\n[编译错误] {e}")
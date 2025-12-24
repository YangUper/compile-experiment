from dataclasses import dataclass
from typing import List


# ==========================================
# 1. 词法分析 (Lexical Analysis)
# ==========================================

@dataclass
class Token:
    type: str
    value: str
    line: int

    def __repr__(self):
        # 方便调试输出
        return f"({self.type}, {self.value})"


class Lexer:
    def __init__(self, code):
        self.code = code
        self.pos = 0
        self.line = 1
        self.KEYWORDS = {'int': 'INT', 'float': 'FLOAT', 'for': 'FOR'}
        self.errors = []  # 新增：用于存储词法错误信息

    def tokenize(self) -> List[Token]:
        tokens = []
        while self.pos < len(self.code):
            ch = self.code[self.pos]

            # 1. 跳过空白符
            if ch.isspace():
                if ch == '\n': self.line += 1
                self.pos += 1
                continue

            # 2. 标识符/关键字 (以字母或下划线开头)
            if ch.isalpha() or ch == '_':
                start = self.pos
                while self.pos < len(self.code) and (self.code[self.pos].isalnum() or self.code[self.pos] == '_'):
                    self.pos += 1
                text = self.code[start:self.pos]
                tokens.append(Token(self.KEYWORDS.get(text, 'ID'), text, self.line))
                continue

            # 3. 数字逻辑 (修改核心：检测非法标识符)
            if ch.isdigit():
                start = self.pos
                # 先读取正常的数字部分
                while self.pos < len(self.code) and (self.code[self.pos].isdigit() or self.code[self.pos] == '.'):
                    self.pos += 1

                # [关键修改]：如果数字读完了，后面紧跟着字母，说明是类似 "2a" 的错误
                if self.pos < len(self.code) and (self.code[self.pos].isalpha() or self.code[self.pos] == '_'):
                    # 继续把后面连着的字母数字全吞掉，作为一个完整的错误Token
                    while self.pos < len(self.code) and (self.code[self.pos].isalnum() or self.code[self.pos] == '_'):
                        self.pos += 1
                    bad_val = self.code[start:self.pos]

                    # 记录错误
                    self.errors.append(f"Line {self.line}: 词法错误 - 无效标识符 '{bad_val}' (不能以数字开头)")
                    tokens.append(Token('ERROR', bad_val, self.line))
                else:
                    # 正常的数字
                    tokens.append(Token('NUM', self.code[start:self.pos], self.line))
                continue

            # 4. 运算符与界符
            two_chars = self.code[self.pos: self.pos + 2]
            if two_chars in ['++', '==', '<=', '>=', '!=']:
                t_type = '++' if two_chars == '++' else 'relop'
                tokens.append(Token(t_type, two_chars, self.line))
                self.pos += 2
                continue

            if ch in '<>':
                tokens.append(Token('relop', ch, self.line))
                self.pos += 1
            elif ch in ['=', '+', '-', '*', '/', ';', '(', ')', '{', '}']:
                tokens.append(Token(ch, ch, self.line))
                self.pos += 1
            else:
                # 处理完全无法识别的字符（如 @, # 等）
                self.errors.append(f"Line {self.line}: 词法错误 - 非法字符 '{ch}'")
                self.pos += 1

        tokens.append(Token('#', '#', self.line))
        return tokens


# ==========================================
# 2. 语义分析辅助：符号表
# ==========================================

class SymbolTable:
    def __init__(self):
        self.symbols = {}

    def declare(self, name, type_name):
        if name in self.symbols:
            raise RuntimeError(f"语义错误: 变量 '{name}' 重复定义")
        self.symbols[name] = type_name

    def lookup(self, name):
        if name not in self.symbols:
            raise RuntimeError(f"语义错误: 变量 '{name}' 未定义")
        return self.symbols[name]


# ==========================================
# 3. 语法+语义分析器 (Parser + Semantic)
# ==========================================

class Compiler:
    def __init__(self, tokens):
        self.tokens = tokens
        self.pos = 0
        self.sym_table = SymbolTable()
        self.temp_count = 0
        self.label_count = 0
        self.tac_code = []

    def match(self, expected):
        curr = self.tokens[self.pos]
        if curr.type == expected:
            self.pos += 1
            return curr
        raise SyntaxError(f"语法错误 (Line {curr.line}): 期望 {expected}, 得到 {curr.type} '{curr.value}'")

    def peek(self):
        return self.tokens[self.pos]

    def emit(self, code):
        self.tac_code.append(code)

    def new_temp(self):
        self.temp_count += 1
        return f"t{self.temp_count}"

    def new_label(self):
        self.label_count += 1
        return f"L{self.label_count}"

    # --- 递归下降函数 ---

    def parse_stmt_list(self):
        while self.peek().type != '#' and self.peek().type != '}':
            # 如果遇到 ERROR 类型的 token，直接跳过并报错，防止 parser 崩溃
            if self.peek().type == 'ERROR':
                print(f"语法分析跳过非法 Token: {self.peek().value}")
                self.pos += 1
                continue
            self.parse_stmt()

    def parse_stmt(self):
        t = self.peek().type
        if t == 'INT':
            self.parse_decl()
        elif t == 'ID':
            self.parse_assign()
        elif t == 'FOR':
            self.parse_for()
        elif t == '{':
            self.parse_block()
        else:
            # 简单的错误恢复：跳过无法识别的 token
            self.pos += 1

    def parse_decl(self):
        self.match('INT')
        name = self.match('ID').value
        self.sym_table.declare(name, 'int')
        if self.peek().type == '=':
            self.match('=')
            val = self.parse_expr()
            self.emit(f"{name} = {val}")
        self.match(';')

    def parse_assign(self):
        name = self.match('ID').value
        self.sym_table.lookup(name)
        self.match('=')
        val = self.parse_expr()
        self.match(';')
        self.emit(f"{name} = {val}")

    def parse_for(self):
        self.match('FOR')
        self.match('(')

        # Init
        if self.peek().type == 'INT':
            self.match('INT')
            name = self.match('ID').value
            self.sym_table.declare(name, 'int')
        else:
            name = self.match('ID').value
            self.sym_table.lookup(name)
        self.match('=')
        val = self.parse_expr()
        self.match(';')
        self.emit(f"{name} = {val}")

        l_start, l_end = self.new_label(), self.new_label()
        self.emit(f"{l_start}:")

        # Cond
        left = self.parse_expr()
        op = self.match('relop').value
        right = self.parse_expr()
        self.match(';')
        t_cond = self.new_temp()
        self.emit(f"{t_cond} = {left} {op} {right}")
        self.emit(f"if {t_cond} == 0 goto {l_end}")

        # Iter (延迟输出)
        iter_buf = []
        old_emit = self.emit
        self.emit = lambda c: iter_buf.append(c)

        it_id = self.match('ID').value
        self.sym_table.lookup(it_id)
        if self.peek().type == '++':
            self.match('++')
            self.emit(f"{it_id} = {it_id} + 1")
        elif self.peek().type == '=':
            self.match('=')
            e_v = self.parse_expr()
            self.emit(f"{it_id} = {e_v}")

        self.emit = old_emit
        self.match(')')

        # Block
        self.parse_block()

        for c in iter_buf: self.emit(c)
        self.emit(f"goto {l_start}")
        self.emit(f"{l_end}:")

    def parse_block(self):
        self.match('{')
        self.parse_stmt_list()
        self.match('}')

    def parse_expr(self):
        res = self.parse_term()
        while self.peek().type in ['+', '-']:
            op = self.match(self.peek().type).value
            right = self.parse_term()
            t = self.new_temp()
            self.emit(f"{t} = {res} {op} {right}")
            res = t
        return res

    def parse_term(self):
        res = self.parse_factor()
        while self.peek().type in ['*', '/']:
            op = self.match(self.peek().type).value
            right = self.parse_factor()
            t = self.new_temp()
            self.emit(f"{t} = {res} {op} {right}")
            res = t
        return res

    def parse_factor(self):
        p = self.peek()
        if p.type == '(':
            self.match('(')
            r = self.parse_expr()
            self.match(')')
            return r
        if p.type in ['ID', 'NUM']:
            val = self.match(p.type).value
            if p.type == 'ID': self.sym_table.lookup(val)
            return val
        raise SyntaxError(f"意外的符号: {p.value}")


# ==========================================
# 4. 执行与输出展示
# ==========================================

if __name__ == "__main__":
    # 测试代码：故意包含 "int 2a" 这个错误
    source_code = """
    int a = 0;
    int a1 = 10;
    for (int i = 0; i < 5; i++) {
        a = a + 1;
    }
    """

    print("=" * 40)
    print("源代码:")
    print(source_code)
    print("=" * 40)

    print("\n--- 1. 词法分析结果 ---")
    lexer = Lexer(source_code)
    try:
        tokens = lexer.tokenize()

        # 1.1 先输出检测到的词法错误
        if lexer.errors:
            print("【警告】检测到词法错误:")
            for err in lexer.errors:
                print(f"  -> {err}")
            print("-" * 20)

        # 1.2 输出所有的Token (包括被标记为ERROR的Token)
        print("Token 流:")
        for i, t in enumerate(tokens):
            # 如果是ERROR类型，用显眼的格式打印
            token_str = f"({t.type}, {t.value})"
            if t.type == 'ERROR':
                token_str = f"!!ERROR({t.value})!!"
            print(f"{token_str:<18}", end='\n' if (i + 1) % 5 == 0 else "")
        print("\n")

        # 只有在Token流中有意义的情况下才继续进行语法分析
        # (通常如果词法错误严重，可能会停止，但这里为了演示，我们继续)
        print("--- 2. 语法 & 语义分析 ---")
        compiler = Compiler(tokens)
        compiler.parse_stmt_list()

        print("\n状态: 语法语义分析完成 (已跳过词法错误的Token)")
        print(f"符号表内容: {compiler.sym_table.symbols}")

        print("\n--- 3. 生成的三地址码 (TAC) ---")
        for line in compiler.tac_code:
            print(line)

    except Exception as e:
        print(f"\n[编译终止] {e}")
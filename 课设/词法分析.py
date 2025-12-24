from dataclasses import dataclass


@dataclass
class Token:
    type: str
    value: str


def char_type(ch):
    if ch.isalpha() or ch == '_':
        return 'LETTER'
    elif ch.isdigit():
        return 'DIGIT'
    elif ch == '.':  # 新增小数点
        return 'DOT'
    elif ch in '+-*/=<>!;(){}':
        return ch
    elif ch.isspace():
        return 'SPACE'
    else:
        return 'OTHER'


# ======================
# 完善后的 DFA 状态转移表
# ======================
DFA = {
    0: {
        'LETTER': 1, 'DIGIT': 2, '+': 3, '=': 5, '<': 6, '>': 6, '!': 6,
        ';': 'SEMI', '(': 'LPAREN', ')': 'RPAREN', '{': 'LBRACE', '}': 'RBRACE',
        '*': 'MUL', '/': 'DIV', 'SPACE': 0
    },
    1: {'LETTER': 1, 'DIGIT': 1},  # ID

    # 状态 2: 整数部分
    2: {'DIGIT': 2, 'DOT': 7, 'LETTER': 99},

    # 状态 7: 遇到小数点（此时还不是合法浮点数，后面必须跟数字）
    7: {'DIGIT': 8, 'LETTER': 99, 'DOT': 99},

    # 状态 8: 合法浮点数态
    8: {'DIGIT': 8, 'LETTER': 99, 'DOT': 99},

    3: {'+': 4},
    5: {'=': 6},
    6: {'=': 6},

    # 错误态 99: 吸收所有后续干扰字符
    99: {'LETTER': 99, 'DIGIT': 99, 'DOT': 99}
}


class DFALexer:
    KEYWORDS = {'int': 'INT', 'for': 'FOR', 'float': 'FLOAT'}

    def __init__(self, code):
        self.code = code
        self.pos = 0
        self.tokens = []
        self.errors = []

    def tokenize(self):
        while self.pos < len(self.code):
            # 跳过空白
            while self.pos < len(self.code) and self.code[self.pos].isspace():
                self.pos += 1
            if self.pos >= len(self.code): break

            state = 0
            lexeme = ''
            start_pos = self.pos

            while self.pos < len(self.code):
                ch = self.code[self.pos]
                ctype = char_type(ch)

                if state not in DFA or ctype not in DFA[state]:
                    break

                nxt = DFA[state][ctype]
                if isinstance(nxt, str):
                    self.tokens.append(Token(nxt, ch))
                    self.pos += 1
                    state = -1
                    break

                state = nxt
                lexeme += ch
                self.pos += 1

            if state == -1: continue

            # ===== 状态检查 =====
            if state == 1:
                self.tokens.append(Token(self.KEYWORDS.get(lexeme, 'ID'), lexeme))
            elif state == 2:
                self.tokens.append(Token('NUM', lexeme))
            elif state == 8:  # 合法浮点数
                self.tokens.append(Token('FLOAT_NUM', lexeme))
            elif state == 7:  # 只有 "3." 这种形式
                self.errors.append(f"错误：无效的浮点数格式 '{lexeme}'")
            elif state == 99:
                self.errors.append(f"错误：词法单元 '{lexeme}' 非法 (数字开头或格式错误)")
            elif state in [3, 4, 5, 6]:  # 处理运算符
                types = {3: '+', 4: '++', 5: '=', 6: 'relop'}
                self.tokens.append(Token(types[state], lexeme))
            else:
                if start_pos == self.pos:
                    self.errors.append(f"错误：无法识别的字符 '{self.code[self.pos]}'")
                    self.pos += 1

        self.tokens.append(Token('#', '#'))
        return self.tokens, self.errors


if __name__ == "__main__":
    code = """
        int 1i = 0;
        for (i = 0; i < 10; i++) {
            i = i * 1;
        }
        """

    lexer = DFALexer(code)
    tokens, errors = lexer.tokenize()

    print("--- 词法分析结果 ---")
    for t in tokens:
        print(f"({t.type}, {t.value})")

    if errors:
        print("\n--- 检测到以下错误 ---")
        for err in errors:
            print(err)
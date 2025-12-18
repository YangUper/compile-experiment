#include <iostream>
#include <string>
#include <cctype>
#include <iomanip>

using namespace std;

// --- 词法分析部分 ---
enum TokenType {
    ID, NUM, ASSIGN, PLUS, MINUS, MUL, DIV, LPAREN, RPAREN, SEMI, ERR, END
};

struct Token {
    TokenType type;
    string value;
};

class Lexer {
    string src;
    size_t pos;
public:
    Lexer(string s) : src(s), pos(0) {}

    Token nextToken() {
        while (pos < src.length() && isspace(src[pos])) pos++; // 跳过空格
        if (pos >= src.length()) return {END, "#"};

        char cur = src[pos];
        if (isalpha(cur)) { // 标识符
            string s;
            while (pos < src.length() && isalnum(src[pos])) s += src[pos++];
            return {ID, s};
        }
        if (isdigit(cur)) { // 数字
            string s;
            while (pos < src.length() && isdigit(src[pos])) s += src[pos++];
            return {NUM, s};
        }
        pos++;
        switch (cur) {
            case '=':
                return {ASSIGN, "="};
            case '+':
                return {PLUS, "+"};
            case '-':
                return {MINUS, "-"};
            case '*':
                return {MUL, "*"};
            case '/':
                return {DIV, "/"};
            case '(':
                return {LPAREN, "("};
            case ')':
                return {RPAREN, ")"};
            case ';':
                return {SEMI, ";"};
            default:
                return {ERR, string(1, cur)};
        }
    }
};

// --- 语法与语义分析部分 ---
class Parser {
    Lexer lexer;
    Token look;        // 当前读入的 Token
    int tempCount = 0; // 临时变量计数器
    bool hasError = false;

    string newTemp() { return "t" + to_string(++tempCount); }

    void match(TokenType expected) {
        if (look.type == expected) look = lexer.nextToken();
        else {
            if (!hasError) {
                cout << ">> [语法错误] 期望符号类型: " << expected << "，实际读入: " << look.value << endl;
                hasError = true;
            }
        }
    }

    // 生成并打印四元式
    void emit(string op, string arg1, string arg2, string res) {
        cout << "[中间代码] (" << left << setw(2) << op << ", "
             << setw(4) << arg1 << ", " << setw(4) << arg2 << ", " << res << ")" << endl;
    }

public:
    Parser(string s) : lexer(s) { look = lexer.nextToken(); }

    // S -> id = E;
    void parseAssignment() {
        if (look.type == ID) {
            string target = look.value;
            match(ID);
            if (look.type == ASSIGN) {
                match(ASSIGN);
                string res = parseE(); // 分析表达式
                emit("=", res, "_", target);
                if (look.type == SEMI) {
                    match(SEMI);
                    if (!hasError) cout << ">>> 状态：该语句符合语法规范。" << endl;
                } else cout << ">> [语法错误] 缺少分号 ';'" << endl;
            } else cout << ">> [语法错误] 缺少赋值号 '='" << endl;
        } else cout << ">> [语法错误] 语句应以标识符开头" << endl;
    }

    // E -> T { (+|-) T }
    string parseE() {
        cout << "  分析表达式 E..." << endl;
        string left = parseT();
        while (look.type == PLUS || look.type == MINUS) {
            string op = look.value;
            match(look.type);
            string right = parseT();
            string t = newTemp();
            emit(op, left, right, t);
            left = t;
        }
        return left;
    }

    // T -> F { (*|/) F }
    string parseT() {
        string left = parseF();
        while (look.type == MUL || look.type == DIV) {
            string op = look.value;
            match(look.type);
            string right = parseF();
            string t = newTemp();
            emit(op, left, right, t);
            left = t;
        }
        return left;
    }

    // F -> (E) | id | num
    string parseF() {
        if (look.type == LPAREN) {
            match(LPAREN);
            string res = parseE();
            match(RPAREN);
            return res;
        } else if (look.type == ID || look.type == NUM) {
            string val = look.value;
            match(look.type);
            return val;
        } else {
            hasError = true;
            return "?";
        }
    }
};

int main() {
    cout << "--- 赋值语句语法与语义分析程序 ---" << endl;
    cout << "请输入一个赋值语句 (例如: ans = (a + b) * 10;):" << endl;
    string input;
    getline(cin, input);

    Parser parser(input);
    parser.parseAssignment();

    return 0;
}
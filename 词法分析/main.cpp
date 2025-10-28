#include "stdio.h"
#include "ctype.h"
#include "string.h"
#include "stdlib.h"

// 单词类型定义
typedef enum {
    KEYWORD, // 关键字
    IDENTIFIER, // 标识符
    NUMBER, // 数字
    OPERATOR, // 运算符
    DELIMITER, // 分隔符
    STRING, // 字符串
    CHARACTER, // 字符
    COMMENT, // 注释
    END_OF_FILE, // 文件末尾
    UNKNOWN // 未知类型
} TokenType;

// 单词结构
typedef struct {
    TokenType type; // 单词类型
    char value[100]; // 单词值
    int line; // 行号
    int column; // 列号
} Token;

// c语言关键字
const char *keywords[] = {
        "auto", "break", "case", "char", "const", "continue", "default",
        "do", "double", "else", "enum", "extern", "float", "for", "goto",
        "if", "int", "long", "register", "return", "short", "signed",
        "sizeof", "static", "struct", "switch", "typedef", "union",
        "unsigned", "void", "volatile", "while"
};

// 运算符
const char *operators[] = {
        "+", "-", "*", "/", "%", "++", "--", "==", "!=", ">", "<", ">=", "<=",
        "&&", "||", "!", "&", "|", "^", "~", "<<", ">>", "=", "+=", "-=", "*=",
        "/=", "%=", "&=", "|=", "^=", "<<=", ">>="
};

// 分隔符
const char *delimiters[] = {
        ",", ";", "(", ")", "[", "]", "{", "}", ".", "->"
};

char *source_code; // 存储整个源代码
int position = 0; // 当前读取位置
int line = 1; // 当前行号
int column = 1; // 当前列号
Token *tokens = NULL; // token数组
int token_count = 0; // token数量
int token_capacity = 0; // token数组容量

// 初始化函数
void init_lexer(const char* filename){
    FILE *file = fopen(filename, "r");
    if (!file){
        printf("无法打开文件：%s\n", filename);
        exit(1);
    }

    // 获取文件大小
    fseek(file, 0, SEEK_END);
    long file_size = ftell(file);
    fseek(file, 0, SEEK_SET);

    // 读取文件内容
    source_code = (char *) malloc(file_size + 1);
    fread(source_code, 1, file_size, file);
    source_code[file_size] = '\0';

    fclose(file);

    // 初始化token数组
    token_capacity = 100;
    tokens = (Token*) malloc(token_capacity * sizeof (Token));
    token_count = 0;
}

// 添加token函数
void add_token(TokenType type, const char *value){
    if (token_count >= token_capacity){
        token_capacity *= 2;
        tokens = (Token*) realloc(tokens, token_capacity * sizeof (Token));
    }

    Token token;
    token.type = type;
    strncpy(token.value, value, sizeof(token.value) - 1);
    token.value[sizeof(token.value) - 1] = '\0';
    token.line = line;
    token.column = column - strlen(value);

    tokens[token_count++] = token;
}

// 检查是否为关键字
int is_keyword(const char *str){
    int num_keywords = sizeof(keywords) / sizeof(keywords[0]);
    for (int i = 0; i < num_keywords; i++){
        if (strcmp(str, keywords[i]) == 0){
            return 1;
        }
    }
    return 0;
}

// 检查是否为运算符
int is_operator(const char *str){
    int num_operators = sizeof(operators) / sizeof(operators[0]);
    for (int i = 0; i < num_operators; i++){
        if (strcmp(str, operators[i]) == 0){
            return 1;
        }
    }
    return 0;
}

// 检查是否为分隔符
int is_delimiter(char c){
    int num_delimiters = sizeof(delimiters) / sizeof(delimiters[0]);
    for (int i = 0; i < num_delimiters; i++){
        if (c == delimiters[i][0]){
            return 1;
        }
    }
    return 0;
}

// 跳过空白字符
void skip_whitespace(){
    while (source_code[position] != '\0' && isspace(source_code[position])){
        if (source_code[position] == '\n'){
            line++;
            column = 1;
        } else{
            column++;
        }
        position++;
    }
}

// 跳过注释
void skip_comment(){
    if (source_code[position] == '/' && source_code[position+1] == '/'){
        // 单行注释
        while (source_code[position] != '\0' && source_code[position] != '\n'){
            position++;
            column++;
        }
    } else if (source_code[position] == '/' && source_code[position + 1] == '*'){
        // 多行注释
        position += 2;
        column += 2;
        while (source_code[position] != '\0' &&
               !(source_code[position] == '*' && source_code[position+1] == '/')){
            if (source_code[position] == '\n'){
                line++;
                column = 1;
            } else{
                column++;
            }
            position++;
        }
        if (source_code[position] == '*'){
            position += 2;
            column += 2;
        }
    }
}

int is_valid_number(const char *str) {
    int dot_count = 0;
    for (int i = 0; str[i] != '\0'; i++) {
        if (str[i] == '.') {
            dot_count++;
            if (dot_count > 1) return 0;
        } else if (!isdigit(str[i])) {
            return 0;
        }
    }
    return 1;
}

// 获取下一个token
Token get_next_token(){
    Token token;
    token.type = UNKNOWN;
    token.value[0] = '\0';
    token.line = line;
    token.column = column;

    // 跳过空白符
    skip_whitespace();

    // 检查是否到达文件末尾
    if (source_code[position] == '\0'){
        token.type = END_OF_FILE;
        strcpy(token.value, "EOF");
        return token;
    }

    // 跳过注释
    if (source_code[position] == '/' && (source_code[position+1] == '/' || source_code[position+1] == '*')){
        skip_comment();
        return get_next_token(); // 递归调用获取下一个token
    }

    // 标识符和关键字
    if (isalpha(source_code[position]) || source_code[position] == '_'){
        char identifier[100] = "";
        int i = 0;

        while (isalnum(source_code[position]) || source_code[position] == '_'){
            identifier[i++] = source_code[position++];
            column++;
        }
        identifier[i] = '\0';

        if (is_keyword(identifier)){
            token.type = KEYWORD;
        } else{
            token.type = IDENTIFIER;
        }
        strcpy(token.value, identifier);
        return token;
    }

    // 数字
    if (isdigit(source_code[position]) ||
        (source_code[position] == '.' && isdigit(source_code[position+1]))){
        char number[100] = "";
        int i = 0;
        int dot_count = 0;

        while (isdigit(source_code[position]) || source_code[position] == '.'){
            if (source_code[position] == '.'){
                dot_count++;
                if (dot_count > 1){
                    break;
                }
            }

            number[i++] = source_code[position++];
            column++;
        }

        number[i] = '\0';

        if (is_valid_number(number)){
            token.type = NUMBER;
        } else{
            token.type = UNKNOWN;
        }
        strcpy(token.value, number);
        return token;
    }

    // 字符串
    if (source_code[position] == '"'){
        char string[100] = "";
        int i = 0;

        string[i++] = source_code[position++];
        column++;

        while (source_code[position] != '\0' && source_code[position] != '"'){
            string[i++] = source_code[position++];
            column++;
        }

        if (source_code[position] == '"'){
            string[i++] = source_code[position++];
            column++;
            string[i] = '\0';
            token.type = STRING;
            strcpy(token.value, string);
            return token;
        } else{
            string[i] = source_code[position];
            token.type = UNKNOWN;
            strcpy(token.value, string);
            return token;
        }

    }

    // 字符
    if (source_code[position] == '\''){
        char character[10] = "";
        int i = 0;

        character[i++] = source_code[position++]; // 添加开头的'
        column++;

        // 检查文件是否提前结束
        if (source_code[position] == '\0'){
            token.type = UNKNOWN;
            strcpy(token.value, character);
            return token;
        }

        // 处理字符内容
        if (source_code[position] == '\\'){
            // 转义字符，例如 '\n', '\\', '\'' 等
            character[i++] = source_code[position++]; // 加入'\'
            column++;

            if (source_code[position] != '\0'){
                character[i++] = source_code[position++]; // 加入转义后的字符
                column++;
            } else{
                token.type = UNKNOWN;
                strcpy(token.value, character);
                return token;
            }
        } else{
            // 普通字符
            character[i++] = source_code[position++];
            column++;
        }

        // 检查结尾的单引号
        if (source_code[position] == '\''){
            character[i++] = source_code[position++];
            column++;
            character[i] = '\0';
            token.type = CHARACTER;
            strcpy(token.value, character);
        } else{
            // 没有结尾引号
            character[i] = '\0';
            token.type = UNKNOWN;
            strcpy(token.value, character);
        }
        return token;
    }

    // 运算符和分隔符
    char symbol[10] = "";
    int i = 0;

    // 尝试匹配最长运算符
    while (source_code[position] != '\0' && !isspace(source_code[position]) &&
           !isalnum(source_code[position]) && source_code[position] != '_' &&
           source_code[position] != '"' && source_code[position] != '\''){

        symbol[i++] = source_code[position++];
        column++;
        symbol[i] = '\0';

        // 尝试向前看是否又更长的运算符
        char next_symbol[10];
        strcpy(next_symbol, symbol);
        next_symbol[i] = source_code[position];
        next_symbol[i+1] = '\0';

        // 如果下一个字符拼起来仍然是运算符，则继续循环
        if (is_operator(next_symbol)){
            continue;
        }

        // 否则检测当前symbol是否为运算符
        if (is_operator(symbol)){
            token.type = OPERATOR;
            strcpy(token.value, symbol);
            return token;
        }

        // 如果只是一个分隔符
        if (i == 1 && is_delimiter(symbol[0])){
            token.type = DELIMITER;
            strcpy(token.value, symbol);
            return token;
        }
        break;
    }

    // 未知字符
    if (i > 0){
        token.type = UNKNOWN;
        strcpy(token.value, symbol);
    }
    return token;
}

// 词法分析主函数
void tokenize(){
    Token token;
    do {
        token = get_next_token();
        if (token.type != END_OF_FILE) {
            add_token(token.type, token.value);
        }
    } while (strcmp(token.value, "EOF") != 0);
}

// 打印所有token
void print_tokens(){
    const char *type_names[] = {
            "KEYWORD", "IDENTIFIER", "NUMBER", "OPERATOR",
            "DELIMITER", "STRING", "CHARACTER", "COMMENT", "END_OF_FILE", "UNKNOWN"
    };

    printf("行号\t列号\t类型\t\t值\n");
    printf("----\t----\t--------\t----------\n");

    for (int i = 0; i < token_count; i++) {
        printf("%d\t%d\t%-12s\t%s\n",
               tokens[i].line, tokens[i].column,
               type_names[tokens[i].type], tokens[i].value);
    }
}

// 释放资源
void free_lexer(){
    free(source_code);
    free(tokens);
}

int main() {
    const char *filename = "./test.txt";
    init_lexer(filename);

    tokenize();
    print_tokens();

    free_lexer();
    return 0;
}

from lexer import lexer
from parser import Parser
from semantico import SemanticAnalyzer

def read_input_file(file_path):
    with open(file_path, 'r') as file:
        return file.read()

def main():
    input_file_path = "/comp/input.txt"
    code = read_input_file(input_file_path)

    tokens = lexer(code)
    parser = Parser(tokens)
    ast = parser.parse()

    semantic_analyzer = SemanticAnalyzer(ast)
    semantic_analyzer.analyze()

    if semantic_analyzer.errors:
        print("Semantic errors found:")
        for error in semantic_analyzer.errors:
            print(error)
    else:
        print("Semantic analysis completed successfully. No errors found.")

if __name__ == "__main__":
    main()

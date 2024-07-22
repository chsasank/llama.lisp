def get_eof():
    stdio_h = open("/usr/include/stdio.h")
    eof_char = None # Set the correct value here if the script cannot determine it
    for line in stdio_h:
        if line.startswith("#define EOF"):
            eof_char = eval(line.split()[2])

    if eof_char is None:
        raise Exception("Could not determine the EOF character for this platform. Please set it manually")
    return eof_char

EOF = ["trunc", get_eof(), "int8"]

if __name__ == "__main__":
    print(f"Determined EOF value: {EOF}")

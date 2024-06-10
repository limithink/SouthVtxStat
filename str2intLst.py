import sys

def str2intLst(s :str):
    strLst = "("
    for c in s:
        strLst += str(ord(c)) + '\x20'
    strLst = strLst.rstrip() + ')'
    return strLst

if __name__ == "__main__":
    if len(sys.argv) > 1 and sys.argv[1] != "-":
        print(str2intLst(sys.argv[1]))
    else:
        sys.stdin.mode="rb"
        byteSrc = sys.stdin.buffer.read()
        print(str2intLst(byteSrc.decode("utf-8")))

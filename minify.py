import sys

def jmpComment(src: bytearray, idx: int) -> int:
    while idx < len(src):
        tempC = src[idx]
        idx += 1
        if tempC == 0x0D or tempC == 0x0A:
            break
    if tempC == 0x0D and src[idx] == 0x0A:
        idx += 1
    return idx

def jmpTab(src: bytearray, idx: int) -> int:
    while idx < len(src):
        if src[idx] != 0x09:
            break
        idx += 1
    return idx

def jmpSpace(src: bytearray, idx: int) -> int:
    while idx < len(src):
        if src[idx] != 0x20:
            break 
        idx += 1
    return idx

def jmp0D0A(src: bytearray, idx: int) -> int:
    while idx < len(src):
        tempC = src[idx]
        if tempC != 0x0D and tempC != 0x0A:
            break
        idx += 1
    return idx

def minify(src: bytes) -> bytes: 
    ret = bytearray()
    srcLen = len(src)
    ctrr = 0
    prevC = b'('
    flagStrFin = True
    while ctrr < srcLen:
        curC = src[ctrr]
        #Exhausting Continuous Non-significant Char
        isMod = False
        while flagStrFin:
            isUpd = False
            #override '\t'
            if curC == 0x09:
                ctrr = jmpTab(src, ctrr + 1)
                isUpd = True
            #override SPACE
            if curC == 0x20:
                ctrr = jmpSpace(src, ctrr + 1)
                isUpd = True
            #override ';'
            if curC == 0x3B:
                ctrr = jmpComment(src, ctrr + 1)
                isUpd = True
            #override '\n' '\r'
            if curC == 0x0D or curC == 0x0A:
                ctrr = jmp0D0A(src, ctrr + 1)
                isUpd = True
            if isUpd:
                if ctrr < srcLen:
                    isMod = True
                    curC = src[ctrr]
                else:
                    curC = None
                    break
            else:
                if isMod and (prevC != curC or curC not in [0x28, 0x29]):
                    isMod = False
                    ret += b'\x20'
                break
        #Trans State of Copying String
        if curC == 0x22:
            if flagStrFin:
                flagStrFin = False
            else:
                flagStrFin = True
            ret += b'\"'
            ctrr += 1
            continue
        #Copying
        prevC = curC
        if curC:
            ret.append(curC)
        ctrr += 1
    return bytes(ret)

if __name__ == "__main__" and len(sys.argv) >= 3:
    fo = open(sys.argv[1], "rb")
    bSrc = fo.read()
    fo.close()
    bRet = minify(bSrc)
    fo = open(sys.argv[2], "wb")
    fo.write(bRet)
    fo.close()

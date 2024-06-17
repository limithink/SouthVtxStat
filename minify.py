import sys

def minify(src: bytes) -> bytes: 
    ret = bytearray()
    srcLen = len(src)
    ctr = 0
    prevC = b'('
    flagStrFin = True
    while ctr < srcLen:
        #Exhausting Continuous Non-significant Char
        isMod = False
        while flagStrFin and ctr < srcLen:
            isUpd = False
            #override '\t'
            if src[ctr] == 0x09:
                ctr += 1
                isUpd = True
                while ctr < srcLen and src[ctr] == 0x09:
                    ctr += 1
            #override SPACE
            if src[ctr] == 0x20:
                ctr += 1
                isUpd = True
                while ctr < srcLen and src[ctr] == 0x20:
                    ctr += 1
            #override Comment followed by ';'
            if src[ctr] == 0x3B:
                ctr += 1
                isUpd = True
                while ctr < srcLen and src[ctr] not in [0x0D, 0x0A]:
                    ctr += 1
                if src[ctr] == 0x0D and src[ctr + 1] == 0x0A:
                    ctr += 2
                else:
                    ctr += 1
            #override '\n' '\r'
            if src[ctr] == 0x0D or src[ctr] == 0x0A:
                ctr += 1
                isUpd = True
                while ctr < srcLen and src[ctr] in [0x0D, 0x0A]:
                    ctr += 1
            #State
            if isUpd:
                isMod = True
            else:
                if isMod and prevC != 0x28 and src[ctr] != 0x29:
                    ret += b'\x20'
                break
        #Trans State of Copying String
        if ctr < srcLen and src[ctr] == 0x22:
            if flagStrFin:
                flagStrFin = False
            else:
                flagStrFin = True
            ret += b'\"'
            ctr += 1
            continue
        #Copying
        if ctr < srcLen:
            prevC = src[ctr]
            ret.append(prevC)
        ctr += 1
    return bytes(ret)

if __name__ == "__main__" and len(sys.argv) >= 3:
    fo = open(sys.argv[1], "rb")
    bSrc = fo.read()
    fo.close()
    bRet = minify(bSrc)
    fo = open(sys.argv[2], "wb")
    fo.write(bRet)
    fo.close()

with open("elements.csv", "r") as elements:

    total = 0
    res = "["
    lines = elements.readlines()
    #print(len(lines))
    for line in lines[1:-2]:
        parts = line.split(",")
        res += "\""+parts[2].lower()+"\""
        res += ";"
    
    res += "\""+lines[-1].split(",")[2].lower()+"\""
    res += "]"
    print(res)

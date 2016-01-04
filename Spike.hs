import PythTree

s = Shape [(-5,-2),(3,1),(-4,4)]

main = putStrLn $ unlines 
    ["\\documentclass[border=10pt]{standalone}"
    ,"\\usepackage{tikz}" 
    , "\\begin{document}"
    , "\\begin{tikzpicture}"
    , drawWithTikz s 
    , "\\end{tikzpicture}"
    , "\\end{document}"]

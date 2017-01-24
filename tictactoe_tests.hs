-- TESTING 
b1 = move (pos1 X) (nb O)
b2 = move (pos2 O) $ move (pos1 X) (nb O)
b3 = move (pos3 X) $ move (pos2 O) $ move (pos1 X) (nb O)

x2win = move (pos5 O) $ move (pos2 X) $ move (pos6 O) $ move (pos1 X) (nb O)
draw = move (pos3 O) $ move (pos4 X) $ move (pos5 O) $ move (pos2 X) $ move (pos6 O) b1
xwon = move (pos3 X) $ x2win

-- fails:
--samePos = move (pos1 X) pos1
--samePlayer = move (pos2 X) $ move (pos1 X) (nb O) 
--afterFinished = move (pos4 O) $ xwon 
--afterDraw = move (pos1 X) $ draw 

whoWonT = whoWon xwon
whoWonDrawt = whoWon draw

-- fails:
--whoWonTF1 = whoWon (nb O)
--whoWonTF2 = whoWon x2win

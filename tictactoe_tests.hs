import TicTacToe

-- TESTING 
b1 = move (p1 X) (new O)
b2 = move (p2 O) $ move (p1 X) (new O)
b3 = move (p3 X) $ move (p2 O) $ move (p1 X) (new O)

draw = move (p7 X) $ move (p8 O) $ move (p9 X) $ move (p3 O) $ move (p6 X) $ move (p5 O) $ move (p2 X) $ move (p4 O) $ move (p1 X) (new O)
x2win = move (p5 O) $ move (p2 X) $ move (p6 O) $ move (p1 X) (new O)
xwon = move (p3 X) $ x2win

-- fails:
--samePos = move (p1 X) b1
--samePlayer = move (p2 X) $ move (p1 X) (new O) 
--afterFinished = move (p4 O) $ xwon 
--afterDraw = move (p1 X) $ draw 

--whoWonT = whoWon xwon
--whoWonDrawt = whoWon draw

-- fails:
--whoWonTF1 = whoWon (new O)
--whoWonTF2 = whoWon x2win

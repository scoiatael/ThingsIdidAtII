package checkers; // This package is required - don't remove it
public class EvaluatePosition { // This class is required - don't remove it
  static private final int WIN = Integer.MAX_VALUE / 2;
  static private final int LOSE = Integer.MIN_VALUE / 2;
  static private boolean _color; // This field is required - don't remove it
  static public void changeColor(boolean color) // This method is required - don't remove it
  {
    _color = color;
  }
  static public boolean getColor() // This method is required - don't remove it
  {
    return _color;
  }

  static int abs(int x)
  {
    if(x < 0) {
      return -x;
    } else {
      return x;
    }
  }

  static int sq(int x)
  {
    return x*x;
  }

  static int lengthSquared(int x1, int y1, int x2, int y2) 
  {
    return sq(x1 - x2) + sq(y1 - y2);
  }

  static int fartherFromCenter(int hSize, int x, int y)
  {
	int vx = abs(x - hSize);
	int vy = abs(y - hSize);
//    return (vx > vy) ? vy : vx;
  	return (vx + vy);
  }

  static int fortifyYouserlf(int[][] pieces, int pieceNumber) {
    int count = 0;
    for (int i=0;i<pieceNumber ;i++ ) {
      for(int j=i+1; j<pieceNumber;j++) {
        count += lengthSquared(pieces[i][0], pieces[i][1], pieces[j][0], pieces[j][1]);
      }
    }
    return count;
  }

  static int closerBetter(int[][] pieces1, int piece1number, int[][] pieces2, int piece2number, int size, AIBoard board)
  {
    int count = 0;
    for(int i=0; i<piece1number; i++) {
      int minLen = size*size*10;
      for(int j=0; j<piece2number; j++) {
        int len = lengthSquared(pieces1[i][0], pieces1[i][1], pieces2[j][0], pieces2[j][1]);
        if(board._board[pieces2[j][0]][pieces2[j][1]].king) {
          if(len < minLen) {
            minLen = len; 
          } 
          count += len;
        }
      }
      count += minLen;
    }
    return count;
  }

  static public int evaluatePosition(AIBoard board) // This method is required and it is the major heuristic method - type your code here
  {

    int myRating = 0;
    int hisRating = 0;
    int size = board.getSize();
    int hsize = size / 2;

    int myPieces[][] = new int [size*size][2];
    int myPieceNumber = 0;
    int myKingNumber = 0;
    int hisPieces[][] = new int [size*size][2];
    int hisPieceNumber = 0;
    int hisKingNumber = 0;

    int PAWN_VAL = 4 * size * size;
    int KING_VAL = 2 * PAWN_VAL;
    int CRITICAL_PIECE_NUMBER = size / 2;
    int POSITION_VS_PIECE_VAL = 8;

    for (int i = 0; i < size; i++) {
      for (int j = (i + 1) % 2; j < size; j += 2) {
        if (!board._board[i][j].empty) { // field is not empty
          if (board._board[i][j].white == getColor()) { // this is my piece
            myPieces[myPieceNumber][0] = i;
            myPieces[myPieceNumber][1] = j;
            myPieceNumber++;
            if (board._board[i][j].king) myKingNumber += 1; // this is my king
          } else {
            hisPieces[hisPieceNumber][0] = i;
            hisPieces[hisPieceNumber][1] = j;
            hisPieceNumber++;
            if (board._board[i][j].king) hisKingNumber += 1; // This is opponent's king
          }
        }
      }
    }

    
    for (int i=0;i<myPieceNumber ;i++ ) {
      myRating += fartherFromCenter(hsize, myPieces[i][0], myPieces[i][1]) / POSITION_VS_PIECE_VAL;
    } 
    for (int i=0;i<hisPieceNumber ;i++ ) {
      hisRating += fartherFromCenter(hsize, hisPieces[i][0], hisPieces[i][1]) / POSITION_VS_PIECE_VAL;
    }
  
    myRating += fortifyYouserlf(hisPieces, hisPieceNumber);  
    hisRating += fortifyYouserlf(myPieces, myPieceNumber);

    int hisPawnNumber = hisPieceNumber - hisKingNumber;
    int myPawnNumber = myPieceNumber - myKingNumber;
    hisRating += hisKingNumber * KING_VAL + hisPawnNumber * PAWN_VAL;
    myRating += myKingNumber * KING_VAL + myPawnNumber * PAWN_VAL;

    //Judge.updateLog("Type your message here, you will see it in the log window\n");
    

    
    if(hisPieceNumber < CRITICAL_PIECE_NUMBER && myPieceNumber >= hisPieceNumber) {
      hisRating += closerBetter(hisPieces, hisPieceNumber, myPieces, myPieceNumber, size, board);
    } else if(myPieceNumber < CRITICAL_PIECE_NUMBER) {
      myRating += closerBetter(myPieces, myPieceNumber, hisPieces, hisPieceNumber, size, board);
    }

    

    if (myRating == 0) return LOSE;
    else if (hisRating == 0) return WIN;
    else return myRating - hisRating;
  }
}




package de.tuberlin.sese.swtpp.gameserver.model.ploy;

import java.util.List;

import de.tuberlin.sese.swtpp.gameserver.model.ploy.Board.MoveUtil;

public class Lance extends Figure {
    /**
	 * 
	 */
	private static final long serialVersionUID = 5815663032775357898L;
	List<Integer> possibleMoves;

	int positionIndex;


    public Lance(boolean isWhite, List<Integer> directions,int index) {
        super(isWhite, directions,index);
    }



    @Override
    void setPossibleMoves() {
      this.possibleMoves =  possibleUnitMoves();
      if(this.possibleMoves == null) System.out.println("possible moves is null");
        possiblePositionMap.put(this,this.possibleMoves);


    }

    @Override
    public List<Integer> getPossibleMoves() {
        return possibleMoves;
    }


    /**
     * @param position set figure position index on board
     */
    @Override
    void setPosition(int position) {
        this.positionIndex = position;
    }
	@Override
	int getPositionIndex() {
		// TODO Auto-generated method stub
		return this.positionIndex;
	}



	@Override
	void move(MoveUtil move) {
		// TODO Auto-generated method stub
		if(move.rotation > 0) {
			rotateFigure(move.rotation);
		}else {
			moveFigure(move);
		}
		
	}
}

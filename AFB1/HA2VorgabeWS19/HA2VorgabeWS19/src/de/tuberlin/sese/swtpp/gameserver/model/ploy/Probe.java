package de.tuberlin.sese.swtpp.gameserver.model.ploy;

import java.util.List;

import de.tuberlin.sese.swtpp.gameserver.model.ploy.Board.MoveUtil;

public class Probe extends Figure  {

    /**
	 * 
	 */
	private static final long serialVersionUID = 1255980866385493882L;
	List<Integer> possibleMoves;

    public Probe(boolean isWhite, List<Integer> directions, int index) {
        super(isWhite, directions, index);
    }
	int positionIndex;


    @Override
    public List<Integer> getPossibleMoves() {
        return possibleMoves;
    }


    @Override
    void setPossibleMoves() {
        this.possibleMoves = possibleUnitMoves();
        possiblePositionMap.put(this,this.possibleMoves);

    }



    /**
     * @param position set figure position
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

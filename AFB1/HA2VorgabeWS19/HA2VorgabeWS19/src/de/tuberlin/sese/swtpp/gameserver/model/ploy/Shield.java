package de.tuberlin.sese.swtpp.gameserver.model.ploy;

import java.util.List;

import de.tuberlin.sese.swtpp.gameserver.model.ploy.Board.MoveUtil;

public class Shield extends Figure  {
    /**
     *
     */
    private static final long serialVersionUID = 4808637449758055428L;

    /**
     * list of possible moves
     */
    List<Integer> possibleMoves;

    /**
     * index on board
     */
    int positionIndex;

    /**
     *  constructor
     * @param isWhite owner
     * @param directions directions
     * @param index position
     */
    public Shield(boolean isWhite, List<Integer> directions, int index) {
        super(isWhite, directions, index);
    }


    /**
     * update legal possible moves
     */
    @Override
    void setPossibleMoves() {
        this.possibleMoves = possibleUnitMoves();
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
        return this.positionIndex;
    }



    @Override
    void move(MoveUtil move) {
        moveFigure(move);
        rotateFigure(move.rotation);
    }

}

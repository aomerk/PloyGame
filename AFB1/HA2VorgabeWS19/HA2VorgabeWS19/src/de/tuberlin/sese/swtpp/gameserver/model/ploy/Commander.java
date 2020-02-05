package de.tuberlin.sese.swtpp.gameserver.model.ploy;

import java.util.ArrayList;
import java.util.List;

import de.tuberlin.sese.swtpp.gameserver.model.ploy.Board.MoveUtil;

public class Commander extends Figure {
	/**
	 *
	 */
	private static final long serialVersionUID = 4494014093651042570L;
	List<Integer> possibleMoves;

	int positionIndex;

	public Commander(boolean isWhite, List<Integer> directions, int index) {

		super(isWhite, directions, index);

	}

	/**
	 * update legal possible moves
	 */
	@Override
	void setPossibleMoves() {
		List<Integer> possiblePositionIndices = new ArrayList<>();
		int index = this.positionIndex;
		for (Integer direction : this.getDirections()) {
			int aimIndex = index * dirToDistance[direction];
			if (checkOutOfBounds(index, aimIndex, 1))
				break;
			if (aimIndex < 0 || aimIndex > 80)
				break;
			if (Board.positions.get(aimIndex).getFigure() != null)
				break;

		}
		this.possibleMoves = possiblePositionIndices;
		possiblePositionMap.put(this, this.possibleMoves);

	}

	@Override
	public List<Integer> getPossibleMoves() {
		return possibleMoves;
	}

	/**
	 * @param position set figure position
	 */
	@Override
	void setPosition(int position) {
		this.positionIndex = position;
		if (isWhite())
			whiteCommanderIndex = position;
		else
			blackCommanderIndex = position;

	}

	@Override
	int getPositionIndex() {
		// TODO Auto-generated method stub
		return this.positionIndex;
	}

	@Override
	void move(MoveUtil move) {
		// TODO Auto-generated method stub
		if (move.rotation > 0) {
			rotateFigure(move.rotation);
		} else {
			moveFigure(move);
		}

	}

}

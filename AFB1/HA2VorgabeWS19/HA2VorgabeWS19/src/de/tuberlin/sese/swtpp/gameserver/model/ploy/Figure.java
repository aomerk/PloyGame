package de.tuberlin.sese.swtpp.gameserver.model.ploy;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

import de.tuberlin.sese.swtpp.gameserver.model.ploy.Board.MoveUtil;

public abstract class Figure implements Serializable {

    /* ////////// Static Figure Properties /////////// */

    /**
     * always know where your commander is
     */
    static int whiteCommanderIndex;

    /**
     * always know where your commander is
     */
    static int blackCommanderIndex;

    /**
     * implementing Serializable interface
     */
    private static final long serialVersionUID = -8346938339558523780L;


    /**
     * Each figure has legal moves
     */
    static HashMap<Figure, List<Integer>> possiblePositionMap = new HashMap<>();

    /**
     * constant to convert direction to index
     */
    final protected static int[] dirToDistance = {-9, -8, 1, 10, 9, 8, -1, -10};


    /* ////////// Figure Properties ////////// */

    /**
     * which player it belongs to
     */
    private final boolean isWhite;

    /**
     * directions, converted from binary string
     */
    private final List<Integer> directions;



    /* ////////// Abstract Methods ////////// */


    /**
     * @param positionIndex set figure position
     */
    abstract void setPosition(int positionIndex);

    /**
     * Each figure has a list of legal positions to go
     *
     * @return List of index
     */
    abstract List<Integer> getPossibleMoves();


    /**
     * general move method, rotate and/or move
     *
     * @param move parsed move information
     */
    abstract void move(MoveUtil move);

    /**
     * @return position to board index
     */
    abstract int getPositionIndex();

    /**
     * update legal possible moves
     */
    abstract void setPossibleMoves();


    /**
     * public constructor
     *
     * @param isWhite       figure owner
     * @param directions    figure directions
     * @param positionIndex figure position
     */
    public Figure(boolean isWhite, List<Integer> directions, int positionIndex) {
        this.isWhite = isWhite;
        this.directions = directions;
        this.setPosition(positionIndex);
    }


    /**
     * Move and update board
     *
     * @param move parsed move information
     */
    void moveFigure(MoveUtil move) {
//        System.out.println("moving figure");
        move.aimPosition.setFigure(move.startPosition.getFigure());
        Board.positions.set(move.aimIndex, move.aimPosition); // aimIndex is aimPosition
        if (move.aimIndex != move.startIndex) move.startPosition.setFigure(); // set starting positions figure to null

        Board.positions.set(move.startIndex, move.startPosition); // set starting index to start position

//        System.out.println("moved figure" + Board.positions.get(move.aimIndex).getFigure());
//        this.setPossibleMoves();

    }

    /**
     * @return figure directions
     */
    public List<Integer> getDirections() {
        return directions;
    }


    /**
     * @return list of integers, legal indexes it can go
     */
    List<Integer> possibleUnitMoves() {
        List<Integer> possiblePositionIndices = new ArrayList<>();
        int index = this.getPositionIndex();
        for (Integer direction : this.getDirections()) {
            for (int distance = 1; distance <= this.getDirections().size(); distance++) {
                int aimIndex = index + (distance * dirToDistance[direction]);
                if (checkOutOfBounds(index, aimIndex, distance)) break;
                Figure aimFig = Board.positions.get(aimIndex).getFigure();
                if (aimFig != null && aimFig.isWhite() == this.isWhite)
                    break;
                if (aimFig != null && aimFig.isWhite() != this.isWhite ) {
                    possiblePositionIndices.add(aimIndex);
                    break;
                }

                possiblePositionIndices.add(aimIndex);
            }
        }
//        System.out.println(this + " " + this.getPositionIndex()+ " "  + possiblePositionIndices);
        return possiblePositionIndices;
    }


    /**
     * since we are handling positions with indexes only, index-10 may look legal
     * but it is actually somewhere stupid like this
     * -----x
     * y-----
     * x,y has 1 index distance but illegal
     * but we not that you cant go more than your distance
     *
     * @param index    row (9,8,7,6..)
     * @param aimIndex column  (a,b,c,d,e,f..)
     * @param distance which way to move
     * @return if aim index is O.K
     */
    boolean checkOutOfBounds(int index, int aimIndex, int distance) {
        int col = (index % 9) + 1, row = 9 - (index / 9);
        int aimCol = (aimIndex % 9) + 1, aimRow = 9 - (aimIndex / 9);
        return (Math.abs(aimRow - row) > distance || Math.abs(aimCol - col) > distance || aimIndex < 0 || aimIndex > 80);
    }


    /**
     * which player does it belong to
     *
     * @return true for figure belongs to white player
     */
    public boolean isWhite() {
        return isWhite;
    }


    /**
     * @return board representation of figure
     */
    @Override
    public String toString() {
        String figureStr = (this.isWhite) ? "w" : "b";
        StringBuilder arrowPositions = new StringBuilder("00000000");
        for (Integer a : this.directions) {
            arrowPositions.setCharAt(arrowPositions.length() - 1 - a, '1');
        }
        int figureDigit = Integer.parseInt(arrowPositions.toString(), 2);
        return figureStr.concat(Integer.toString(figureDigit));
    }

    /**
     * @param figureStr String to turn into object
     * @return passing figure object
     */
    static Figure stringToFigure(String figureStr, int index) {
        if (figureStr.length() < 2)
            return null;
        boolean isWhite = figureStr.startsWith("w"); // set player
        List<Integer> directions = new ArrayList<>(); // list of directions

        for (int i = 7, idx = 0; i >= 0; i--, idx++) {
            if (figureStringToBinary(figureStr).charAt(i) == '1')
                directions.add(idx); // if it has one at an index, this is a direction
        }

        //set figure type according to directions size
        FigureFactory figureFactory = new FigureFactory();
        return figureFactory.getFigure(isWhite, directions, index);
    }


    /**
     * @param figureStr figure string to extract directions
     * @return 1s are reversed index for directions
     */
    static String figureStringToBinary(String figureStr) {
        figureStr = figureStr.substring(1); // string after player code
        String figureBinary = Integer.toBinaryString(Integer.parseInt(figureStr)); // digit to binary
        while (figureBinary.length() != 8) {
            figureBinary = "0".concat(figureBinary); // while string not 8 bit sized, add 0 to front
        }
        return figureBinary;
    }


    /**
     * using modulus to not to become > 7
     *
     * @param rotate how much to rotate
     */
    void rotateFigure(int rotate) {
//        System.out.println("rotating" + this.directions);
        for (int i = 0; i < this.directions.size(); i++) {
            this.directions.set(i, (directions.get(i) + rotate) % 8);
        }
//        System.out.println("rotated" + this.directions);
    }

    void normalMove(MoveUtil move) {
        if (move.rotation > 0) {
            rotateFigure(move.rotation);
        } else
            moveFigure(move);
    }


}

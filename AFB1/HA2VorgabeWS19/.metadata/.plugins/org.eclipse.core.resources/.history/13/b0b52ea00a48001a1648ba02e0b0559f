package de.tuberlin.sese.swtpp.gameserver.model.ploy;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class Board implements Serializable {

    /**
     * Regex for checking move validity
     */
    final String regex = "[a-z][0-9]-[a-z][0-9]-[0-7]";

    /*
     * List of all positions
     * */
    static List<Position> positions = new ArrayList<>();

    /**
     *
     */
    private static final long serialVersionUID = 7653957898077245748L;

    /**
     * game state with figures and positions as string
     */
    private String state;


    /**
     * initialize board with both empty positions and positions with figures on it
     */
    public Board() {
        updateState(
                ",w84,w41,w56,w170,w56,w41,w84,/,,w24,w40,w17,w40,w48,,/,,,w16,w16,w16,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,b1,b1,b1,,,/,,b3,b130,b17,b130,b129,,/,b69,b146,b131,b170,b131,b146,b69,");
    }


    /**
     * @return String representation of game
     */
    String getState() {
        return state;
    }


    /**
     * @param moveString    move in fen representation
     * @param isWhitePlayer which player tries to move
     * @return if move is possible
     */
    boolean checkMove(String moveString, boolean isWhitePlayer) {
        boolean returns = true;
        if (!moveString.matches(regex)) return false; // is a valid move
        MoveUtil move = moveParser(moveString);
        Figure figure = positions.get(move.startIndex).getFigure();

        if (figure == null) return false;
        if (figure.isWhite() != isWhitePlayer) returns = false; // does figure belong to player?
        if ((move.startIndex == move.aimIndex) && move.rotation != 0) return true; // you gotta do something, right?
        System.out.println(returns);

        if (!checkObligedMovesSatisfied(isWhitePlayer, move)) returns = false; // there is another move you must make
        System.out.println(returns);

        returns = figure.getPossibleMoves().contains(move.aimIndex); // is move a valid rule
        System.out.println(returns);
        return returns;
    }

    /**
     * @param whitePlays whose turn is it?
     * @param move       to check it satisfies obligations
     * @return does it satisfy obligations
     */
    boolean checkObligedMovesSatisfied(boolean whitePlays, MoveUtil move) {
        for (Map.Entry<Figure, List<Integer>> entry : Figure.possiblePositionMap.entrySet()) {
            // if opponent can reach your commander and it is your turn, you must escape
            int index = (whitePlays) ? Figure.whiteCommanderIndex : Figure.blackCommanderIndex;
            boolean figureIsWhite = entry.getKey().isWhite();
            boolean keyOwner = (whitePlays) == figureIsWhite;
            if (whitePlays && keyOwner && entry.getValue().contains(index) && move.startIndex != index)
                return false;
        }
        return true;
    }

    /**
     *
     */
    static class MoveUtil implements Serializable {
        /**
         *
         */
        private static final long serialVersionUID = -8477094112570126176L;
        int startIndex;
        int aimIndex;
        int rotation;
        Position startPosition;
        Position aimPosition;
    }

//

    /**
     * @param moveString <start> <destination> <rotation>
     * @return parsed MoveUtil object
     */
    MoveUtil moveParser(String moveString) {
        MoveUtil move = new MoveUtil();
        String[] moveToken = moveString.split("-"); // tokenize move input
        move.startIndex = Position.positionToIndex(moveToken[0]); // startIndex index
        move.aimIndex = Position.positionToIndex(moveToken[1]); // aimIndex index
        move.rotation = Integer.parseInt(moveToken[2]);
        move.startPosition = positions.get(move.startIndex); // get startIndex position and figure to move
        move.aimPosition = positions.get(move.aimIndex); // aiming position
        return move;
    }

    /**
     * @param moveString <start> <destination> <rotation>
     * @return finished game
     */
    boolean makeMove(String moveString) {
        MoveUtil move = moveParser(moveString);
        Figure figure = move.startPosition.getFigure();

        if (positions.get(move.aimIndex).getFigure() instanceof Commander) {

            figure.move(move);
            updateState(this.toString());

            return true;
        }


//		System.out.println("want to move " + figure + " to " + move.aimIndex + "  " + figure.getPossibleMoves());
        /*
         * Shield can first move then rotate
         */
        figure.move(move);
        updateState(this.toString());
//		System.out.println("possible moves of figures " + Figure.possiblePositionMap.size());
        return false;
    }

    /**
     * contact positions and figures into string
     *
     * @return FEN representation of our OOP logic
     */
    @Override
    public String toString() {
        String state = "";
        for (int i = 0; i < positions.size(); i++) {
            Position pos = positions.get(i);
            if (i % 9 != 0)
                state = state.concat(",");
            if (pos.getFigure() != null) {
                state = state.concat(pos.getFigure().toString());
            }
            if (i % 9 == 8 && i != positions.size() - 1)
                state = state.concat("/");
        }
        return state;
    }


    /**
     * take string and turn it into OOP Representation
     *
     * @param state string
     */
    void updateState(String state) {
        if (this.state != null && !this.state.equals(state)) {
            notifyAllObservers();

        }
        this.state = state;
        String st = this.state.replaceAll("/", ","); // small trick to count beginnings too
        String[] figuresArray = st.split(",", -1); // tokenize figure elements

        positions = new ArrayList<>();
        for (int i = 0; i < figuresArray.length; i++) {
            positions.add(i, new Position(figuresArray[i], i)); // add positions to all
//			System.out.println(i + "  " + positions.get(i).getIndex());
        }
//		for(Position position : positions) {
//			System.out.println("i " + position.positionToIndex());
//		}
        notifyAllObservers();

    }

    void notifyAllObservers() {
        Figure.possiblePositionMap = new HashMap<>();
        for (Position position : positions) {
            if (position.getFigure() != null) {
                position.getFigure().setPossibleMoves();
            }

        }
    }
}

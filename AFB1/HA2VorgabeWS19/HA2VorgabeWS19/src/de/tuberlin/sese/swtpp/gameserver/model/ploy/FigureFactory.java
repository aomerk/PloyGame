package de.tuberlin.sese.swtpp.gameserver.model.ploy;

import java.util.List;

/**
 * Factory method class
 */
public class FigureFactory {

    /**
     *
     * @param isWhite owner
     * @param directions directions
     * @param index position
     * @return new figure with correct subclass
     */
    public Figure getFigure(boolean isWhite, List<Integer> directions, int index){
        switch (directions.size()) {
            case 1:
                return new Shield(isWhite, directions, index);
            case 2:
                return new Probe(isWhite, directions, index);
            case 3:
                return new Lance(isWhite, directions, index);
            case 4:
                return new Commander(isWhite, directions, index);
            default:
                return null;
        }
    }
}

package de.tuberlin.sese.swtpp.gameserver.model.ploy;


import java.io.Serializable;

public class Position implements Serializable {

    /**
     *
     */
    private static final long serialVersionUID = 8758994040408071927L;


    /**
     * @return Which Figure stands on this position
     */
    public Figure getFigure() {
        return figure;
    }

    /**
     *
     * @param figure set what figure stands here
     */
    public void setFigure(Figure figure) {
        this.figure = figure;
    }

    /**
     * no figure stands here
     */
    public void setFigure() {
        this.figure = null;
    }

    private int row;
    private int col;
    private int index;
    private Figure figure;

//    @Override
//    public boolean equals(Object o) {
//        if (this == o) return true;
//        if (o == null || getClass() != o.getClass()) return false;
//        Position position = (Position) o;
//        return row == position.row &&
//                col == position.col;
//    }


    public Position(String figureStr, int index) {
        setPositionFromIndex(index);
        this.figure = Figure.stringToFigure(figureStr, index);

    }



    /**
     * 105 is ASCII 'i' , 94 is ASCII 'a' , 105 mod a = 9 and so on...
     *
     * @return position to index
     */
    public static int positionToIndex(String position) {
        char col = position.charAt(0);
        int row = Character.getNumericValue(position.charAt(1));
        int colEncoded = 9 - (105 % ((int) col));
        return ((9 - row) * 9) + colEncoded - 1;
    }

    /**
     * @param index in array
     */
    public void setPositionFromIndex(int index) {
        this.col = (index % 9) + 1;
        this.row = 9 - ( index / 9);
        this.index = index;
    }




}

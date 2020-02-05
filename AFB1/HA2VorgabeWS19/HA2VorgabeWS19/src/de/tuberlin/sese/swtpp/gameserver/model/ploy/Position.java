package de.tuberlin.sese.swtpp.gameserver.model.ploy;


import java.io.Serializable;

class Position implements Serializable {


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

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        Position position = (Position) o;
        return row == position.row &&
                col == position.col;
    }


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
        // position b1 olsun
        char col = position.charAt(0); // bu b
        int row = Character.getNumericValue(position.charAt(1)); // bu 1
        int colEncoded = 9 - (105 % ((int) col)); // bu a yi 0 a ceviriyor. a = 0 b = 1 c = 2 ... i = 8
        return ((9 - row) * 9) + colEncoded - 1; // bu normal index mantigi
    }

    /**
     * @param index in array
     */
    public void setPositionFromIndex(int index) {
        this.col = (index % 9) + 1;
        this.row = 9 - (index / 9);
        this.index = index;
    }


    public int getIndex() {
        return this.index;
    }


}

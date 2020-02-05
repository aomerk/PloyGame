package de.tuberlin.sese.swtpp.gameserver.test.ploy;

import static org.junit.Assert.assertEquals;

import de.tuberlin.sese.swtpp.gameserver.control.GameFactory;
import de.tuberlin.sese.swtpp.gameserver.model.Bot;
import de.tuberlin.sese.swtpp.gameserver.model.HaskellBot;
import org.junit.Before;
import org.junit.Test;

import de.tuberlin.sese.swtpp.gameserver.control.GameController;
import de.tuberlin.sese.swtpp.gameserver.model.Player;
import de.tuberlin.sese.swtpp.gameserver.model.User;
import de.tuberlin.sese.swtpp.gameserver.model.ploy.PloyGame;

public class TryMoveTest {

    User user1 = new User("Alice", "alice");
    User user2 = new User("Bob", "bob");

    Player whitePlayer = null;
    Player blackPlayer = null;
    PloyGame game = null;
    GameController controller;

    @Before
    public void setUp() throws Exception {
        controller = GameController.getInstance();
        controller.clear();

        int gameID = controller.startGame(user1, "", "ploy");

        game = (PloyGame) controller.getGame(gameID);
        blackPlayer = game.getPlayer(user1);

    }

    public void startGame(String initialBoard, boolean whiteNext) {
        controller.joinGame(user2, "ploy");
        whitePlayer = game.getPlayer(user2);

        game.setBoard(initialBoard);
        game.setNextPlayer(whiteNext ? whitePlayer : blackPlayer); // not changed for white first
    }


    public void assertMove(String move, boolean white, boolean expectedResult) {
        if (white)
            assertEquals(expectedResult, game.tryMove(move, whitePlayer));
        else
            assertEquals(expectedResult, game.tryMove(move, blackPlayer));
    }

    public void assertGameState(String expectedBoard, boolean whiteNext, boolean finished, boolean whiteWon) {
        String board = game.getBoard().replaceAll("e", "");

        assertEquals(expectedBoard, board);
        assertEquals(finished, game.isFinished());

        if (!game.isFinished()) {
            assertEquals(whiteNext, game.isWhiteNext());
        } else {
            assertEquals(whiteWon, whitePlayer.isWinner());
            assertEquals(!whiteWon, blackPlayer.isWinner());
        }
    }

    /*******************************************
     * !!!!!!!!! To be implemented !!!!!!!!!!!!
     *******************************************/

    @Test
    public void moveTest() {
        startGame(
                ",w84,w41,w56,w170,w56,w41,w84,/,,w24,w40,w17,w40,w48,,/,,,w16,w16,w16,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,b1,b1,b1,,,/,,b3,b130,b17,b130,b129,,/,b69,b146,b131,b170,b131,b146,b69,",
                false);
//		assertMove("c8-c6-0", true, true);
        assertMove("h1-h4-0", false, true);
        assertGameState(
                ",w84,w41,w56,w170,w56,w41,w84,/,,w24,w40,w17,w40,w48,,/,,,w16,w16,w16,,,/,,,,,,,,/,,,,,,,,/,,,,,,,b69,/,,,b1,b1,b1,,,/,,b3,b130,b17,b130,b129,,/,b69,b146,b131,b170,b131,b146,,",
                true, false, false);
    }

    @Test
    public void rotationTest() {
        startGame(
                ",w84,w41,w56,w170,w56,w41,w84,/,,w24,w40,w17,w40,w48,,/,,,w16,w16,w16,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,b1,b1,b1,,,/,,b3,b130,b17,b130,b129,,/,b69,b146,b131,b170,b131,b146,b69,",
                false);
        assertMove("e3-e4-1", false, true);

        assertGameState(
                ",w84,w41,w56,w170,w56,w41,w84,/,,w24,w40,w17,w40,w48,,/,,,w16,w16,w16,,,/,,,,,,,,/,,,,,,,,/,,,,b2,,,,/,,,b1,,b1,,,/,,b3,b130,b17,b130,b129,,/,b69,b146,b131,b170,b131,b146,b69,",
                true, false, false);
    }

    @Test
    public void onlyRotationTest() {
        startGame(
                ",w84,w41,w56,w170,w56,w41,w84,/,,w24,w40,w17,w40,w48,,/,,,w16,w16,w16,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,b1,b1,b1,,,/,,b3,b130,b17,b130,b129,,/,b69,b146,b131,b170,b131,b146,b69,",
                false);
        assertMove("e3-e3-1", false, true);

        assertGameState(
                ",w84,w41,w56,w170,w56,w41,w84,/,,w24,w40,w17,w40,w48,,/,,,w16,w16,w16,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,b1,b2,b1,,,/,,b3,b130,b17,b130,b129,,/,b69,b146,b131,b170,b131,b146,b69,",
                true, false, false);
    }

    @Test
    public void finishedGameTest() {
        startGame(
                ",w84,w41,w56,,w56,w41,w84,/,,w24,w40,w17,w40,w48,,/,,,w16,w16,w16,,,/,,,,,,,,/,,,,,,,,/,,,,w170,,,,/,,,b1,b1,b1,,,/,,b3,b130,b17,b130,b129,,/,b69,b146,b131,b170,b131,b146,b69,",
                false);


        assertMove("e3-e4-0", false, true);

        assertGameState(
                ",w84,w41,w56,,w56,w41,w84,/,,w24,w40,w17,w40,w48,,/,,,w16,w16,w16,,,/,,,,,,,,/,,,,,,,,/,,,,b1,,,,/,,,b1,,b1,,,/,,b3,b130,b17,b130,b129,,/,b69,b146,b131,b170,b131,b146,b69,",
                true, true, false);
    }

    @Test
    public void badMoveString() {
        startGame(
                ",w84,w41,w56,,w56,w41,w84,/,,w24,w40,w17,w40,w48,,/,,,w16,w16,w16,,,/,,,,,,,,/,,,,,,,,/,,,,w170,,,,/,,,b1,b1,b1,,,/,,b3,b130,b17,b130,b129,,/,b69,b146,b131,b170,b131,b146,b69,",
                false);


        assertMove("e3-ssse4-0", false, false);

        assertGameState(
                ",w84,w41,w56,,w56,w41,w84,/,,w24,w40,w17,w40,w48,,/,,,w16,w16,w16,,,/,,,,,,,,/,,,,,,,,/,,,,w170,,,,/,,,b1,b1,b1,,,/,,b3,b130,b17,b130,b129,,/,b69,b146,b131,b170,b131,b146,b69,",
                false, false, false);
    }

    @Test
    public void moveNotFigureTest() {
        startGame(
                ",w84,w41,w56,,w56,w41,w84,/,,w24,w40,w17,w40,w48,,/,,,w16,w16,w16,,,/,,,,,,,,/,,,,,,,,/,,,,w170,,,,/,,,b1,b1,b1,,,/,,b3,b130,b17,b130,b129,,/,b69,b146,b131,b170,b131,b146,b69,",
                false);


        assertMove("a9-a9-0", false, false);

        assertGameState(
                ",w84,w41,w56,,w56,w41,w84,/,,w24,w40,w17,w40,w48,,/,,,w16,w16,w16,,,/,,,,,,,,/,,,,,,,,/,,,,w170,,,,/,,,b1,b1,b1,,,/,,b3,b130,b17,b130,b129,,/,b69,b146,b131,b170,b131,b146,b69,",
                false, false, false);
    }

    @Test
    public void moveNotOwnedTest() {
        startGame(
                ",w84,w41,w56,,w56,w41,w84,/,,w24,w40,w17,w40,w48,,/,,,w16,w16,w16,,,/,,,,,,,,/,,,,,,,,/,,,,w170,,,,/,,,b1,b1,b1,,,/,,b3,b130,b17,b130,b129,,/,b69,b146,b131,b170,b131,b146,b69,",
                false);

        assertMove("c1-c1-0", false, false);


        assertGameState(
                ",w84,w41,w56,,w56,w41,w84,/,,w24,w40,w17,w40,w48,,/,,,w16,w16,w16,,,/,,,,,,,,/,,,,,,,,/,,,,w170,,,,/,,,b1,b1,b1,,,/,,b3,b130,b17,b130,b129,,/,b69,b146,b131,b170,b131,b146,b69,",
                false, false, false);
    }

    @Test
    public void notMovingTest() {
        startGame(
                ",w84,w41,w56,,w56,w41,w84,/,,w24,w40,w17,w40,w48,,/,,,w16,w16,w16,,,/,,,,,,,,/,,,,,,,,/,,,,w170,,,,/,,,b1,b1,b1,,,/,,b3,b130,b17,b130,b129,,/,b69,b146,b131,b170,b131,b146,b69,",
                false);


        assertMove("b9-b9-0", false, false);

        assertGameState(
                ",w84,w41,w56,,w56,w41,w84,/,,w24,w40,w17,w40,w48,,/,,,w16,w16,w16,,,/,,,,,,,,/,,,,,,,,/,,,,w170,,,,/,,,b1,b1,b1,,,/,,b3,b130,b17,b130,b129,,/,b69,b146,b131,b170,b131,b146,b69,",
                false, false, false);
    }

    @Test
    public void obligedMoveTest() {
        startGame(
                ",w84,w41,w56,,w56,w41,w84,/,,w24,w40,w17,w40,w48,,/,,,w16,w16,w16,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,b1,w170,b1,,,/,,b3,b130,b17,b130,b129,,/,b69,b146,b131,b170,b131,b146,b69,",
                true);


        assertMove("d7-d6-0", true, false);

        assertGameState(
                ",w84,w41,w56,,w56,w41,w84,/,,w24,w40,w17,w40,w48,,/,,,w16,w16,w16,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,b1,w170,b1,,,/,,b3,b130,b17,b130,b129,,/,b69,b146,b131,b170,b131,b146,b69,",
                true, false, false);
    }

    // TODO: implement test cases of same kind as example here
}
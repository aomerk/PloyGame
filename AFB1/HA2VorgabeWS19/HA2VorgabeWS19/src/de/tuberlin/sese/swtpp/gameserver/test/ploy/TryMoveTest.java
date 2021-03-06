package de.tuberlin.sese.swtpp.gameserver.test.ploy;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import java.util.ArrayList;

import org.junit.Before;
import org.junit.Test;

import de.tuberlin.sese.swtpp.gameserver.control.GameController;
import de.tuberlin.sese.swtpp.gameserver.model.Player;
import de.tuberlin.sese.swtpp.gameserver.model.Statistics;
import de.tuberlin.sese.swtpp.gameserver.model.User;
import de.tuberlin.sese.swtpp.gameserver.model.ploy.PloyGame;
import de.tuberlin.sese.swtpp.gameserver.model.ploy.Position;
import de.tuberlin.sese.swtpp.gameserver.model.Move;
import de.tuberlin.sese.swtpp.gameserver.model.Game;
import de.tuberlin.sese.swtpp.gameserver.model.ploy.Board;
import de.tuberlin.sese.swtpp.gameserver.model.ploy.Commander;



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
        game.setNextPlayer(whiteNext? whitePlayer:blackPlayer);
    }

    public void assertMove(String move, boolean white, boolean expectedResult) {
        if (white)
            assertEquals(expectedResult, game.tryMove(move, whitePlayer));
        else
            assertEquals(expectedResult,game.tryMove(move, blackPlayer));
    }

    public void assertGameState(String expectedBoard, boolean whiteNext, boolean finished, boolean whiteWon) {
        String board = game.getBoard().replaceAll("e", "");

        assertEquals(expectedBoard,board);
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
    public void exampleTest() {
        startGame(",w84,w41,w56,w170,w56,w41,w84,/,,w24,w40,w17,w40,w48,,/,,,w16,w16,w16,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,b1,b1,b1,,,/,,b3,b130,b17,b130,b129,,/,b69,b146,b131,b170,b131,b146,b69,",false);
        assertMove("e3-e4-0",false,true);
        assertGameState(",w84,w41,w56,w170,w56,w41,w84,/,,w24,w40,w17,w40,w48,,/,,,w16,w16,w16,,,/,,,,,,,,/,,,,,,,,/,,,,b1,,,,/,,,b1,,b1,,,/,,b3,b130,b17,b130,b129,,/,b69,b146,b131,b170,b131,b146,b69,",true,false,false);
    }
    @Test
    public void playerNotOnTurnTest(){
        startGame(",w84,w41,w56,w170,w56,w41,w84,/,,w24,w40,w17,w40,w48,,/,,,w16,w16,w16,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,b1,b1,b1,,,/,,b3,b130,b17,b130,b129,,/,b69,b146,b131,b170,b131,b146,b69,",false);
        assertMove("e3-e4-0",true,false);
        assertGameState(",w84,w41,w56,w170,w56,w41,w84,/,,w24,w40,w17,w40,w48,,/,,,w16,w16,w16,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,b1,b1,b1,,,/,,b3,b130,b17,b130,b129,,/,b69,b146,b131,b170,b131,b146,b69,",false,false,false);

    }
    //TODO: implement test cases of same kind as example here
//	@Test
//	public void PositionTest() {
//		Position pos1 = new Position("105", 3);
//		assertEquals(pos1.getIndex(), 3);
//		assertEquals(pos1.equals(pos1), true);
//		Position pos2 = null;
//		assertEquals(pos1.equals(pos2), false);
//		Position pos3 = new Position("105",3);
//		assertEquals(pos1.equals(pos3), true);
//		Player notPosition = null;
//		assertEquals(pos1.equals(notPosition), false);
//		Player notPosition2 = new Player(user1, game);
//		assertEquals(pos1.equals(notPosition2), false);
//		Position pos4 = new Position("100", 7);
//		assertEquals(pos1.equals(pos4), false);
//		Position pos5 = new Position("105", 83);
//		assertEquals(pos1.equals(pos5), false);
//
//	}
//	@Test
//	public void UserTest() {
//		user1.setName("alice");
//		assertEquals(user1.getName(), "alice");
//		user1.setId("test");
//		assertEquals(user1.getId(), "test");
//		user1.setPwdhash("test");
//		assertEquals(user1.getPwdhash(), "test");
//	}
//
//	@Test
//	public void PlayerTest() {
//		Player player1 = new Player(user1, game);
//		Player player2 = new Player(user2, game);
//
//		player1.surrender();
//		assertEquals(player1.surrendered(), true);
//		assertEquals(game.gameInfo(), "");
//
//		player2.requestDraw();
//		assertEquals(player2.requestedDraw(), true);
//
//		user1.setName("Alice");
//		assertEquals(player1.getName(), "Alice");
//		User user3 =null;
//		Player player3 = new Player(user3, game);
//		assertEquals(player3.getName(), "");
//		player3.invalidateGame();
//		assertEquals(player3.isGameInvalid(), true);
//
//
//
//	}
//	@Test
//	public void StatisticsTest() {
//		Statistics stats = new Statistics();
//		stats.numWon = 0;
//		stats.numDraw = 0;
//		stats.numLost = 0;
//
//		int y = (int) stats.avgPoints();
//		assertEquals(y, 0);
//		stats.numWon = 2;
//		stats.numDraw = 0;
//		stats.numLost = 1;
//		int x = (int) stats.avgPoints();
//		assertEquals(x, 2);
//	}
//	@Test
//	public void moveClassTest() {
//		Move test = new Move("w84"," ", whitePlayer);
//		assertEquals(test.getState(), " ");
//		test.setMove("w84");
//		assertEquals(test.getMove(),"w84");
//		test.setPlayer(blackPlayer);
//		assertEquals(test.getPlayer(), blackPlayer);
//
//
//	}
//
//	@Test
//	public void gameTest() {
//		PloyGame test = new PloyGame();
//		Player player1 = new Player(user1,test);
//		Player player2 = new Player(user2,test);
//		assertEquals(test.giveUp(player1), false);
//		test.addPlayer(player1);
//		test.addPlayer(player2);
//		assertEquals(test.addPlayer(player1),false);
//		assertEquals(test.getMaxPlayers(),2);
//		assertEquals(test.getMinPlayers(),2);
//		assertEquals(test.getStatus(), "Started");
//		test.setError(true);
//		assertEquals(test.isError(),true);
//		assertEquals(test.getStatus(), "Error");
//		test.setError(false);
//		//assertEquals(test.gameInfo(), "");
//		PloyGame  test2 = new PloyGame();
//		assertEquals(test2.getStatus(), "Wait");
//
//		test2.addPlayer(player1);
//		test2.addPlayer(player2);
//
//
//		assertEquals(test2.didWhiteDraw(), false);
//		assertEquals(test2.didBlackDraw(), false);
//		assertEquals(test2.whiteGaveUp(), false);
//		assertEquals(test2.blackGaveUp(), false);
//		player2.requestDraw();
//		assertEquals(test2.gameInfo(), "white called draw");
//
//		player2.surrender();
//		test2.giveUp(player2);
//		assertEquals(test2.whiteGaveUp(), true);
//		assertEquals(test2.gameInfo(),"white gave up" );
//		assertEquals(test2.getStatus(), "Surrendered");
//		assertEquals(test2.regularGameEnd(player1), false);
//		test.giveUp(player1);
//		assertEquals(test.blackGaveUp(),true);
//		assertEquals(test.gameInfo(), "black gave up");
//		assertEquals(test.isDeleted(), false);
//		test.setDeleted(true);
//		assertEquals(test.isDeleted(), true);
//
//	}
//	@Test
//	public void PloyGameTest() {
//		PloyGame test = new PloyGame();
//		Player player1 = new Player(user1,test);
//		Player player2 = new Player(user2,test);
//		test.addPlayer(player1);
//		test.addPlayer(player2);
//
//		player1.requestDraw();
//		assertEquals(test.gameInfo(), "black called draw");   //black draw white not
//		test.callDraw(player2);								  //both draw
//		assertEquals(test.gameInfo(), "draw game");
//		assertEquals(test.giveUp(player1), false);
//
//
//		player2.requestDraw();
//		test.callDraw(player1);
//
//		assertEquals(test.didBlackDraw(),true);
//
//		assertEquals(test.didWhiteDraw(),true);
//		assertEquals(test.getStatus(), "Draw");
//
//
//		if(!test.isWhiteNext()) {
//			assertEquals(test.nextPlayerString(), "b");
//			assertEquals(test.isPlayersTurn(player1), true);
//			assertEquals(test.isPlayersTurn(player2),false);
//			assertEquals(test.tryMove("w84", player2), false);
//			assertEquals(test.isUsersTurn(user1),false); //game finished
//			assertEquals(test.isUsersTurn(user2), false);
//			test.setNextPlayer(player2);
//			assertEquals(test.nextPlayerString(), "w");
//		}
//
//
//	}

    @Test
    public void rotateOnlyTest() {
        startGame(
                ",w84,w41,w56,w170,w56,w41,w84,/,,w24,w40,w17,w40,w48,,/,,,w16,w16,w16,,,/,,,,,,,,/,,,,,,,,/,,,,b1,,,,/,,,b1,,b1,,,/,,b3,b130,b17,b130,b129,,/,b69,b146,b131,b170,b131,b146,b69,",
                false);
        assertMove("e4-e4-1", false, true);
        assertGameState(
                ",w84,w41,w56,w170,w56,w41,w84,/,,w24,w40,w17,w40,w48,,/,,,w16,w16,w16,,,/,,,,,,,,/,,,,,,,,/,,,,b2,,,,/,,,b1,,b1,,,/,,b3,b130,b17,b130,b129,,/,b69,b146,b131,b170,b131,b146,b69,",
                true, false, false);
        assertMove("e8-e8-1",true,true);
        assertGameState(
                ",w84,w41,w56,w170,w56,w41,w84,/,,w24,w40,w34,w40,w48,,/,,,w16,w16,w16,,,/,,,,,,,,/,,,,,,,,/,,,,b2,,,,/,,,b1,,b1,,,/,,b3,b130,b17,b130,b129,,/,b69,b146,b131,b170,b131,b146,b69,",
                false, false, false);

        assertMove("e1-e1-1",false,true);
        assertGameState(
                ",w84,w41,w56,w170,w56,w41,w84,/,,w24,w40,w34,w40,w48,,/,,,w16,w16,w16,,,/,,,,,,,,/,,,,,,,,/,,,,b2,,,,/,,,b1,,b1,,,/,,b3,b130,b17,b130,b129,,/,b69,b146,b131,b85,b131,b146,b69,",
                true, false, false);


    }


    @Test
    public void eatOpponentTest() {
        startGame(
                ",w84,w41,w56,w170,w56,w41,w84,/,,w24,w40,w17,w40,w48,,/,,,w16,w16,,,,/,,,,,,,,/,,,,,,,,/,,,,w16,,,,/,,,b1,b1,b1,,,/,,b3,b130,b17,b130,b129,,/,b69,b146,b131,b170,b131,b146,b69,",
                false);
        assertMove("e3-e4-0", false, true);
        assertGameState(
                ",w84,w41,w56,w170,w56,w41,w84,/,,w24,w40,w17,w40,w48,,/,,,w16,w16,,,,/,,,,,,,,/,,,,,,,,/,,,,b1,,,,/,,,b1,,b1,,,/,,b3,b130,b17,b130,b129,,/,b69,b146,b131,b170,b131,b146,b69,",
                true, false, false);
    }
    @Test
    public void tryToEatYourFigureTest() {
        startGame(
                ",w84,w41,w56,w170,w56,w41,w84,/,,w24,w40,w17,w40,w48,,/,,,w16,w16,,,,/,,,,,,,,/,,,,,,,,/,,,,w16,,,,/,,,b1,b1,b1,,,/,,b3,b130,b17,b130,b129,,/,b69,b146,b131,b170,b131,b146,b69,",
                false);
        assertMove("d2-d3-0", false, false);
        assertGameState(
                ",w84,w41,w56,w170,w56,w41,w84,/,,w24,w40,w17,w40,w48,,/,,,w16,w16,,,,/,,,,,,,,/,,,,,,,,/,,,,w16,,,,/,,,b1,b1,b1,,,/,,b3,b130,b17,b130,b129,,/,b69,b146,b131,b170,b131,b146,b69,",
                false, false, false);
    }

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

    //    @Test
//    public void obligedMoveTest() {
//        startGame(
//                ",w84,w41,w56,,w56,w41,w84,/,,w24,w40,w17,w40,w48,,/,,,w16,w16,w16,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,b1,w170,b1,,,/,,b3,b130,b17,b130,b129,,/,b69,b146,b131,b170,b131,b146,b69,",
//                true);
//
//
//        assertMove("d7-d6-0", true, false);
//
//        assertGameState(
//                ",w84,w41,w56,,w56,w41,w84,/,,w24,w40,w17,w40,w48,,/,,,w16,w16,w16,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,b1,w170,b1,,,/,,b3,b130,b17,b130,b129,,/,b69,b146,b131,b170,b131,b146,b69,",
//                true, false, false);
//    }
    @Test
    public void finishedNoMoreFigureGameTest() {
        startGame(
                "w16,,,,,,,,/b1,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,,w170,,,,/,,,,b1,b1,,,/,,b3,b130,b17,b130,b129,,/,b69,b146,b131,b170,b131,b146,b69,",
                false);


        assertMove("a8-a9-0", false, true);

        assertGameState(
                "b1,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,,w170,,,,/,,,,b1,b1,,,/,,b3,b130,b17,b130,b129,,/,b69,b146,b131,b170,b131,b146,b69,",
                true, true, false);
    }

    @Test
    public void startFinishedMGame() {
        startGame(
                "w16,,,,,,,,/b1,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,,b1,b1,,,/,,b3,b130,b17,b130,b129,,/,b69,b146,b131,b170,b131,b146,b69,",
                false);


    }
    @Test
    public void freeCommanderMove() {
        startGame(
                "w16,,,,,,,,/b1,,,,,,,,/b170,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,,w85,,,,/,,,,b1,b1,,,/,,b3,b130,b17,b130,b129,,/,b69,b146,b131,,b131,b146,b69,",
                true);


        assertMove("e4-e3-0", true, true);

        assertGameState(
                "w16,,,,,,,,/b1,,,,,,,,/b170,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,,w85,b1,,,/,,b3,b130,b17,b130,b129,,/,b69,b146,b131,,b131,b146,b69,",
                false, false, false);
    }
//    @Test
//	public void finishGame(){
//		PloyGame game = new PloyGame();
//		Player player1 = new Player(user1,game);
//		Player player2 = new Player(user2,game);
//		game.addPlayer(player1);
//		game.addPlayer(player2);
//		game.regularGameEnd(player1);
//		assertTrue(game.isFinished());
//		assertTrue(player1.isWinner());
//		assertEquals(game.gameInfo(),"black won");
//
//
//		PloyGame game2 = new PloyGame();
//		player1 = new Player(user1,game);
//		player2 = new Player(user2,game);
//		game2.addPlayer(player1);
//		game2.addPlayer(player2);
//		game2.regularGameEnd(player2);
//		assertTrue(game2.isFinished());
//		assertTrue(player2.isWinner());
//		assertEquals(game2.gameInfo(),"white won");
//
//		assertEquals(game2.getStatus(), "Finished");
//
//
//
//
//	}
//    @Test
//    public void setCommanderPositionIndexTest() {
//    	ArrayList<Integer> directions = new ArrayList<Integer>();
//    	directions.add(0);
//    	directions.add(2);
//    	directions.add(4);
//    	directions.add(6);
//    	Commander commander = new Commander(true,directions,5);
//    	assertEquals(5,commander.getPositionIndex());
//    }
}

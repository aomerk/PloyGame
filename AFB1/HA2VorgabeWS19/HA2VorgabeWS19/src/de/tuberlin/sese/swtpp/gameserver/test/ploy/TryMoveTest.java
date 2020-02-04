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

		int gameID = controller.startGame(user1, "BOTOMER", "ploy");

		game = (PloyGame) controller.getGame(gameID);
		blackPlayer = game.getPlayer(user1);

	}

	public void startGame(String initialBoard, boolean whiteNext) {
		controller.joinGame(user2, "ploy");
		whitePlayer = game.getPlayer(user2);

		game.setBoard(initialBoard);
		game.setNextPlayer(whiteNext ? whitePlayer : blackPlayer); // not changed for white first
	}

	public void assertBotMove(String move){
		HaskellBot bot = (HaskellBot) GameFactory.createBot("haskell", this.game);
		startGame(
				",w84,w41,w56,w170,w56,w41,w84,/,,w24,w40,w17,w40,w48,,/,,,w16,w16,w16,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,b1,b1,b1,,,/,,b3,b130,b17,b130,b129,,/,b69,b146,b131,b170,b131,b146,b69,",
				false);
		assertMove("c2-c4-0", false, true);

		assert bot != null;
		System.out.println("botname" + bot.getName());
		bot.run();

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
	public void exampleTest() {
		startGame(
				",w84,w41,w56,w170,w56,w41,w84,/,,w24,w40,w17,w40,w48,,/,,,w16,w16,w16,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,b1,b1,b1,,,/,,b3,b130,b17,b130,b129,,/,b69,b146,b131,b170,b131,b146,b69,",
				true);
//		assertMove("c8-c6-0", true, true);
		assertMove("c2-c4-0", false, true);
		assertBotMove("e3-e4-0");

		assertGameState(
				",w84,w41,w56,w170,w56,w41,w84,/,,w24,w40,w17,w40,w48,,/,,,w16,w16,w16,,,/,,,,,,,,/,,,,,,,,/,,,,b1,,,,/,,,b1,,b1,,,/,,b3,b130,b17,b130,b129,,/,b69,b146,b131,b170,b131,b146,b69,",
				true, false, false);

	}
	@Test
	public void botTest() {
		startGame(
				",w84,w41,w56,w170,w56,w41,w84,/,,w24,w40,w17,w40,w48,,/,,,w16,w16,w16,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,b1,b1,b1,,,/,,b3,b130,b17,b130,b129,,/,b69,b146,b131,b170,b131,b146,b69,",
				true);

		assertBotMove("e3-e4-0");

		assertGameState(
				",w84,w41,w56,w170,w56,w41,w84,/,,w24,w40,w17,w40,w48,,/,,,w16,w16,w16,,,/,,,,,,,,/,,,,,,,,/,,,,b1,,,,/,,,b1,,b1,,,/,,b3,b130,b17,b130,b129,,/,b69,b146,b131,b170,b131,b146,b69,",
				true, false, false);

	}

	// TODO: implement test cases of same kind as example here
}
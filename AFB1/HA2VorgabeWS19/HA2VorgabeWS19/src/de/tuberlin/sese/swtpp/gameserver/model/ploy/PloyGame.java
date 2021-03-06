package de.tuberlin.sese.swtpp.gameserver.model.ploy;

import java.io.Serializable;

import de.tuberlin.sese.swtpp.gameserver.model.Game;
import de.tuberlin.sese.swtpp.gameserver.model.Move;
import de.tuberlin.sese.swtpp.gameserver.model.Player;

/**
 * Class Cannon extends the abstract class Game as a concrete game instance that
 * allows to play Cannon.
 */
public class PloyGame extends Game implements Serializable {

	/**
	 *
	 */
	private static final long serialVersionUID = 5424778147226994452L;

	/************************
	 * member
	 ***********************/

	// just for better comprehensibility of the code: assign white and black player
	private Player blackPlayer;
	private Player whitePlayer;

	// internal representation of the game state
	/**
	 * Ploy has a 9x9 Board to play
	 */
	private Board board;

	/**
	 * game type
	 */
	String type = "ploy";

	/************************
	 * constructors
	 ***********************/

	public PloyGame() {
		super();
		this.board = new Board();
	}

	public String getType() {
		return "ploy";
	}

	/*******************************************
	 * Game class functions already implemented
	 ******************************************/

	@Override
	public boolean addPlayer(Player player) {
		if (!started) {
			players.add(player);

			// game starts with two players
			if (players.size() == 2) {
				started = true;
				this.blackPlayer = players.get(0);
				this.whitePlayer = players.get(1);
				nextPlayer = blackPlayer;
			}
			return true;
		}

		return false;
	}

	@Override
	public String getStatus() {
		if (error)
			return "Error";
		if (!started)
			return "Wait";
		if (!finished)
			return "Started";
		if (surrendered)
			return "Surrendered";
		if (draw)
			return "Draw";

		return "Finished";
	}

	@Override
	public String gameInfo() {
		String gameInfo = "";

		if (started) {
			if (blackGaveUp())
				gameInfo = "black gave up";
			else if (whiteGaveUp())
				gameInfo = "white gave up";
			else if (didWhiteDraw() && !didBlackDraw())
				gameInfo = "white called draw";
			else if (!didWhiteDraw() && didBlackDraw())
				gameInfo = "black called draw";
			else if (draw)
				gameInfo = "draw game";
			else if (finished)
				gameInfo = blackPlayer.isWinner() ? "black won" : "white won";
		}

		return gameInfo;
	}

	@Override
	public String nextPlayerString() {
		return isWhiteNext() ? "w" : "b";
	}

	@Override
	public int getMinPlayers() {
		return 2;
	}

	@Override
	public int getMaxPlayers() {
		return 2;
	}

	@Override
	public boolean callDraw(Player player) {

		// save to status: player wants to call draw
		if (this.started && !this.finished) {
			player.requestDraw();
		} else {
			return false;
		}

		// if both agreed on draw:
		// game is over
		if (players.stream().allMatch(Player::requestedDraw)) {
			this.draw = true;
			finish();
		}
		return true;
	}

	@Override
	public boolean giveUp(Player player) {
		if (started && !finished) {
			if (this.whitePlayer == player) {
				whitePlayer.surrender();
				blackPlayer.setWinner();
			}
			if (this.blackPlayer == player) {
				blackPlayer.surrender();
				whitePlayer.setWinner();
			}
			surrendered = true;
			finish();

			return true;
		}

		return false;
	}

	/**
	 * @return True if it's white player's turn
	 */
	public boolean isWhiteNext() {
		return nextPlayer == whitePlayer;
	}

	/**
	 * Ends game after regular move (save winner, finish up game state,
	 * histories...)
	 *
	 * @param winner player wins game
	 * @return true if game ends
	 */
	public boolean regularGameEnd(Player winner) {
		// public for tests
		if (finish()) {
			winner.setWinner();
			return true;
		}
		return false;
	}

	public boolean didWhiteDraw() {
		return whitePlayer.requestedDraw();
	}

	public boolean didBlackDraw() {
		return blackPlayer.requestedDraw();
	}

	public boolean whiteGaveUp() {
		return whitePlayer.surrendered();
	}

	public boolean blackGaveUp() {
		return blackPlayer.surrendered();
	}

	//	/*******************************************
//	 * !!!!!!!!! To be implemented !!!!!!!!!!!!
//	 ******************************************/
//	/**
//	 * Sets any given state (String representation) to the concrete game (internal
//	 * representation).
//	 *
//	 * @param boardFEN Board Fen
//	 */
	@Override
	public void setBoard(String boardFEN) {
//		if (this.board == null) {
//			this.board = new Board();
//		} else {
		this.board.updateState(boardFEN);
//		}

	}

	/**
	 * Returns a String representation of the game state (Zustand), e.g. a String
	 * describing the figures of the game board and its figures/positions.
	 *
	 * @return Board Fen
	 */
	@Override
	public String getBoard() {
		// TODO: implement and replace dummy with actual board
		return this.board.getState();
	}

	/**
	 * This method checks if the supplied move is possible to perform in the current
	 * game state/status and, if so, does it. The following has to be checked/might
	 * be changed: - it has to be checked if the move can be performed ---- it is a
	 * valid move ---- it is done by the right player ---- there is no other move
	 * that the player is forced to perform - if the move can be performed, the
	 * following has to be done: ---- the board state has to be updated (e.g.
	 * figures moved/deleted) ---- the board status has to be updated (check if game
	 * is finished) ---- the next player has to be set (if move is over, it's next
	 * player's turn) ---- history is updated
	 *
	 * @param moveString String representation of move
	 * @param player     The player that tries the move
	 * @return true if the move was performed
	 */
	@Override
	public boolean tryMove(String moveString, Player player) {

		if (!player.equals(nextPlayer)) {
//			System.out.println(player + "  " + nextPlayer);
			return false; // done by the right player
		}
		Move move = new Move(moveString, getBoard(), player);

		if (!board.checkMove(moveString, isWhiteNext())) {
//			System.out.println("check move returned false");
			return false; // move not possible

		}
		setNextPlayer((blackPlayer.equals(player) ? whitePlayer : blackPlayer));

		// make Move is true if game finished
		if (board.makeMove(moveString)) {
			this.finish();
			player.setWinner();
		}

		// TODO: update history DONE
		move.setBoard(getBoard());
		this.getHistory().add(move);

		return true;

	}
}

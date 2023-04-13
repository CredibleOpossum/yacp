const NO_CASTLING: Castling = Castling {
    queen_side: false,
    king_side: false,
};
const BOARD_SIZE: i32 = 8;
const NO_VECT: Location = Location { x: 0, y: 0 };
const QUEEN_SIDE_ROOK_X: i32 = 0;
const KING_SIDE_ROOK_X: i32 = 7;

#[derive(PartialEq)]
pub enum BoardState {
    InProgress,
    Checkmate,
    Stalemate,
}

#[derive(Copy, Clone, PartialEq, Debug)]
pub struct Location {
    pub x: i32,
    pub y: i32,
}

#[derive(Copy, Clone, PartialEq, Debug)]
pub enum MoveType {
    Capture,
    Relocation,
    Castle,
    LongMove,
    EnPassant,
    None,
}
#[derive(Copy, Clone, PartialEq)]
pub struct ChessMove {
    pub origin: Location,
    pub destination: Location,
    pub move_type: MoveType,
    pub is_legal: bool,
}

#[derive(Copy, Clone, PartialEq, Debug)]
pub struct Castling {
    pub queen_side: bool,
    pub king_side: bool,
}

#[derive(Copy, Clone, PartialEq, Debug)]
pub enum ChessColor {
    White,
    Black,
    None,
}

#[derive(Copy, Clone, PartialEq, Debug)]
pub enum RookType {
    QueenSide,
    KingSide,
    Unknown,
}

#[derive(Copy, Clone, PartialEq, Debug)]
pub enum ChessPiece {
    None,
    Pawn(ChessColor), // if pawn can be en passant'd
    Knight(ChessColor),
    Bishop(ChessColor),
    Rook(ChessColor, RookType),
    Queen(ChessColor),
    King(ChessColor),
}

impl From<usize> for Location {
    fn from(val: usize) -> Self {
        Location {
            x: (val % BOARD_SIZE as usize) as i32,
            y: (val / BOARD_SIZE as usize) as i32,
        }
    }
}

pub fn usize_to_location(position: usize) -> Location {
    Location {
        x: (position % BOARD_SIZE as usize) as i32,
        y: (position / BOARD_SIZE as usize) as i32,
    }
}

pub fn get_opposite_color(color: ChessColor) -> ChessColor {
    match color {
        ChessColor::White => ChessColor::Black,
        ChessColor::Black => ChessColor::White,
        ChessColor::None => ChessColor::None,
    }
}

#[derive(Copy, Clone, PartialEq, Debug)]
pub struct ChessPosition {
    pub turn: ChessColor,
    pub pieces: [ChessPiece; 64],
    pub castle_data: [Castling; 2], // White | Black
    pub en_passant: Option<Location>,
}

pub fn location_to_usize(location: Location) -> usize {
    (location.x + location.y * 8) as usize
}

impl ChessPosition {
    pub fn get_move_index(&self, chess_move: ChessMove) -> Option<u8> {
        let possible_move = self.get_legal_moves().iter().position(|&r| r == chess_move);

        match possible_move {
            Some(index) => Some(index as u8),
            None => None,
        }
    }
    pub fn make_chess_move_from_index(&mut self, chess_move_index: u8) -> ChessMove {
        let chess_move = self.get_legal_moves()[chess_move_index as usize];
        *self = self.make_move(chess_move);
        chess_move
    }
    pub fn get_board_state(&self) -> BoardState {
        if self.get_legal_moves().is_empty() && self.is_in_check(self.turn) {
            return BoardState::Checkmate;
        }
        if self.get_legal_moves().is_empty() {
            return BoardState::Stalemate;
        }
        BoardState::InProgress
    }
    pub fn make_move_if_possible(&mut self, from: Location, to: Location) -> ChessMove {
        let attempted_move = self.get_move_data(from, to);
        if attempted_move.is_legal {
            *self = self.make_move(attempted_move);
        }
        attempted_move
    }
    pub fn get_move_data(&self, from: Location, to: Location) -> ChessMove {
        let mut is_semi_legal = false;
        let mut temp_move = ChessMove {
            origin: NO_VECT.into(),
            destination: NO_VECT.into(),
            move_type: MoveType::None,
            is_legal: false,
        };
        for pseudo_legal_move in self.get_piece_moves(from, self.turn, true) {
            if pseudo_legal_move.destination == to {
                temp_move = pseudo_legal_move;
                is_semi_legal = true;
            }
        }
        if !is_semi_legal {
            return temp_move;
        }
        let possible_board = self.make_move(temp_move);
        temp_move.is_legal = !possible_board.is_in_check(get_opposite_color(possible_board.turn));
        temp_move
    }
    pub fn get_legal_moves(&self) -> Vec<ChessMove> {
        let mut moves = Vec::new();
        for piece_index in 0..self.pieces.len() {
            for attempted_move in self.get_piece_moves(piece_index.into(), self.turn, true) {
                if self
                    .get_move_data(attempted_move.origin, attempted_move.destination)
                    .is_legal
                {
                    moves.push(attempted_move);
                }
            }
        }
        moves
    }
    pub fn is_in_check(&self, color: ChessColor) -> bool {
        let king_position = self.find_king(color);
        self.is_under_attack(king_position.unwrap(), get_opposite_color(color))
    }
    pub fn find_king(&self, color: ChessColor) -> Option<Location> {
        for (index, chess_piece) in self.pieces.iter().enumerate() {
            if let ChessPiece::King(king_color) = chess_piece {
                if color == *king_color {
                    return Some(usize_to_location(index));
                }
            }
        }
        None
    }
    pub fn is_under_attack(&self, position: Location, attacking_color: ChessColor) -> bool {
        for (index, piece) in self.pieces.iter().enumerate() {
            if get_piece_color(*piece) == attacking_color {
                for possible_move in
                    self.get_piece_moves(usize_to_location(index), attacking_color, false)
                {
                    if possible_move.destination == position {
                        return true;
                    }
                }
            }
        }
        false
    }
    pub fn make_move(&self, chess_move: ChessMove) -> ChessPosition {
        let mut temp_position = *self;
        let moved_piece = temp_position.get_piece(chess_move.origin);
        let index = match get_piece_color(moved_piece) {
            ChessColor::White => 0,
            ChessColor::Black => 1,
            ChessColor::None => panic!(),
        };

        if let ChessPiece::King(_) = moved_piece {
            temp_position.castle_data[index] = NO_CASTLING;
        };

        temp_position.turn = get_opposite_color(self.turn);
        temp_position.set_piece(chess_move.destination, self.get_piece(chess_move.origin));
        temp_position.set_piece(chess_move.origin, ChessPiece::None);
        if chess_move.move_type == MoveType::Castle {
            let distance_difference = chess_move.destination.x - chess_move.origin.x;
            let king_direction = distance_difference / distance_difference.abs();
            let rook_location = match king_direction {
                1 => Location {
                    x: chess_move.destination.x + king_direction,
                    y: chess_move.destination.y,
                },
                -1 => Location {
                    x: chess_move.destination.x + king_direction * 2,
                    y: chess_move.destination.y,
                },
                _ => panic!(),
            };
            let rook_destination = Location {
                x: chess_move.destination.x - king_direction,
                y: chess_move.destination.y,
            };
            temp_position.set_piece(rook_destination, temp_position.get_piece(rook_location));
            temp_position.set_piece(rook_location, ChessPiece::None);
        }
        let direction = match get_piece_color(temp_position.get_piece(chess_move.destination)) {
            ChessColor::White => 1,
            ChessColor::Black => -1,
            _ => panic!(),
        };
        temp_position.en_passant = None;
        if chess_move.move_type == MoveType::LongMove {
            temp_position.en_passant = Some(chess_move.destination);
        };
        if chess_move.move_type == MoveType::EnPassant {
            let mut target = chess_move.destination;
            target.y += direction;
            temp_position.set_piece(target, ChessPiece::None);
        }

        match moved_piece {
            ChessPiece::Rook(_, side) => match side {
                RookType::QueenSide => temp_position.castle_data[index].queen_side = false,
                RookType::KingSide => temp_position.castle_data[index].king_side = false,
                _ => {}
            },
            ChessPiece::Pawn(color) => {
                let back_rank = match color {
                    ChessColor::White => 0,
                    ChessColor::Black => 7,
                    _ => panic!(),
                };
                if chess_move.destination.y == back_rank {
                    temp_position.set_piece(chess_move.destination, ChessPiece::Queen(color))
                }
            }
            _ => {}
        }

        temp_position
    }
    fn is_empty(&self, position: Location) -> bool {
        self.get_piece(position) == ChessPiece::None
    }
    pub fn get_piece(&self, position: Location) -> ChessPiece {
        self.pieces[location_to_usize(position)]
    }
    fn set_piece(&mut self, position: Location, piece: ChessPiece) {
        self.pieces[location_to_usize(position)] = piece;
    }
    pub fn get_piece_moves(
        &self,
        position: Location,
        turn: ChessColor,
        include_castling: bool,
    ) -> Vec<ChessMove> {
        let mut moves: Vec<ChessMove> = Vec::new();
        if turn != get_piece_color(self.get_piece(position)) {
            return moves;
        }
        let straight_moves = vec![[0, 1], [0, -1], [-1, 0], [1, 0]];
        let diagonal_moves = vec![[-1, 1], [1, -1], [-1, -1], [1, 1]];
        let knight_moves = [
            [-1, -2],
            [1, -2],
            [-2, -1],
            [2, -1],
            [-2, 1],
            [2, 1],
            [-1, 2],
            [1, 2],
        ];
        let queen_moves = [straight_moves.clone(), diagonal_moves.clone()].concat();
        let piece = self.get_piece(position);
        let piece_color = get_piece_color(piece);
        match piece {
            ChessPiece::None => return moves,
            ChessPiece::Pawn(_) => {
                let direction = match piece_color {
                    ChessColor::White => -1,
                    ChessColor::Black => 1,
                    _ => panic!(),
                };
                let starting_rank = match piece_color {
                    ChessColor::White => 6,
                    ChessColor::Black => 1,
                    _ => panic!(),
                };
                let mut target = position;
                target.y += direction;
                if vaild_position(target) && self.is_empty(target) {
                    moves.push(ChessMove {
                        origin: position,
                        destination: target,
                        move_type: MoveType::Relocation,
                        is_legal: true,
                    });
                    target.y += direction;
                    let pawn_on_starting_rank = position.y == starting_rank;
                    let is_empty_and_free_square = vaild_position(target) && self.is_empty(target);
                    if pawn_on_starting_rank && is_empty_and_free_square {
                        moves.push(ChessMove {
                            origin: position,
                            destination: target,
                            move_type: MoveType::LongMove,
                            is_legal: true,
                        });
                    }
                }
                for possible_capture in [[-1, direction], [1, direction]] {
                    target = Location {
                        x: position.x + possible_capture[0],
                        y: position.y + possible_capture[1],
                    };
                    if vaild_position(target)
                        && get_opposite_color(get_piece_color(self.get_piece(target)))
                            == piece_color
                    {
                        moves.push(ChessMove {
                            origin: position,
                            destination: target,
                            move_type: MoveType::Capture,
                            is_legal: true,
                        });
                    }
                }
                for possible_enpassant in [-1, 1] {
                    let mut target = position;
                    target.x += possible_enpassant;
                    if vaild_position(target) {
                        match self.en_passant {
                            Some(en_passant_square) => {
                                if en_passant_square == target {
                                    target.y += direction;
                                    moves.push(ChessMove {
                                        origin: position,
                                        destination: target,
                                        move_type: MoveType::EnPassant,
                                        is_legal: true,
                                    });
                                }
                            }
                            _ => {}
                        }
                    }
                }
            }
            ChessPiece::Knight(_) => {
                for possible_move in knight_moves {
                    let target = Location {
                        x: position.x + possible_move[0],
                        y: position.y + possible_move[1],
                    };
                    if vaild_position(target) {
                        if get_piece_color(self.get_piece(target))
                            == get_opposite_color(piece_color)
                        {
                            moves.push(ChessMove {
                                origin: position,
                                destination: target,
                                move_type: MoveType::Capture,
                                is_legal: true,
                            });
                        }
                        if get_piece_color(self.get_piece(target)) == ChessColor::None {
                            moves.push(ChessMove {
                                origin: position,
                                destination: target,
                                move_type: MoveType::Relocation,
                                is_legal: true,
                            });
                        }
                    }
                }
            }
            ChessPiece::Bishop(_) => return mover(self, position, diagonal_moves, turn),
            ChessPiece::Rook(_, _) => return mover(self, position, straight_moves, turn),
            ChessPiece::Queen(_) => return mover(self, position, queen_moves, turn),
            ChessPiece::King(_) => {
                for possible_move in [
                    [-1, -1],
                    [0, -1],
                    [1, -1],
                    [-1, 0],
                    [1, 0],
                    [-1, 1],
                    [0, 1],
                    [1, 1],
                ] {
                    let mut target = position;
                    target.x += possible_move[0];
                    target.y += possible_move[1];
                    if vaild_position(target) {
                        if get_piece_color(self.get_piece(target))
                            == get_opposite_color(piece_color)
                        {
                            moves.push(ChessMove {
                                origin: position,
                                destination: target,
                                move_type: MoveType::Capture,
                                is_legal: true,
                            });
                        }
                        if get_piece_color(self.get_piece(target)) == ChessColor::None {
                            moves.push(ChessMove {
                                origin: position,
                                destination: target,
                                move_type: MoveType::Relocation,
                                is_legal: true,
                            });
                        }
                    }
                }

                let castling_data = match piece_color {
                    ChessColor::White => self.castle_data[0],
                    ChessColor::Black => self.castle_data[1],
                    _ => panic!(),
                };

                // The include castling is to prevent a infinite recursion problem where the "in_under_attack" function
                // will end up infinitely trying to call itself in an attempt to find out if something is being attacked
                // by a castle, which is not possible
                if include_castling && !self.is_in_check(turn) {
                    let possible = [castling_data.queen_side, castling_data.king_side];
                    let direction = [-1, 1];
                    let length = [3, 2];
                    for side in 0..2 {
                        if possible[side] {
                            let mut can_castle = true;
                            let mut target = position;
                            for _ in 0..length[side] {
                                target.x += direction[side];
                                if !vaild_position(target) {
                                    break;
                                }
                                let is_under_attack =
                                    !self.is_under_attack(target, get_opposite_color(piece_color));
                                if !is_under_attack || self.get_piece(target) != ChessPiece::None {
                                    can_castle = false;
                                }
                            }
                            target.x += direction[side];
                            match self.get_piece(target) {
                                ChessPiece::Rook(_, _) => {}
                                _ => can_castle = false,
                            };

                            if can_castle {
                                let mut king_destination = position;
                                king_destination.x += 2 * direction[side];
                                moves.push(ChessMove {
                                    origin: position,
                                    destination: king_destination,
                                    move_type: MoveType::Castle,
                                    is_legal: true,
                                })
                            }
                        }
                    }
                }
            }
        };

        moves
    }
}

pub fn vaild_position(click_position: Location) -> bool {
    (0..BOARD_SIZE).contains(&click_position.x) && (0..BOARD_SIZE).contains(&click_position.y)
}

fn mover(
    board_state: &ChessPosition,
    position: Location,
    directions: Vec<[i32; 2]>,
    turn: ChessColor,
) -> Vec<ChessMove> {
    let mut moves: Vec<ChessMove> = Vec::new();
    for possible_move in directions {
        let mut target = position;
        loop {
            target.x += possible_move[0];
            target.y += possible_move[1];
            if vaild_position(target) {
                if get_piece_color(board_state.get_piece(target)) == turn {
                    break;
                }
                if turn == get_opposite_color(get_piece_color(board_state.get_piece(target))) {
                    moves.push(ChessMove {
                        origin: position,
                        destination: target,
                        move_type: MoveType::Capture,
                        is_legal: true,
                    });
                    break;
                }
                moves.push(ChessMove {
                    origin: position,
                    destination: target,
                    move_type: MoveType::Relocation,
                    is_legal: true,
                });
            } else {
                break;
            }
        }
    }
    moves
}

pub fn get_piece_color(piece: ChessPiece) -> ChessColor {
    match piece {
        ChessPiece::None => ChessColor::None,
        ChessPiece::Pawn(color)
        | ChessPiece::Knight(color)
        | ChessPiece::Bishop(color)
        | ChessPiece::Rook(color, _)
        | ChessPiece::Queen(color)
        | ChessPiece::King(color) => color,
    }
}

pub fn fen_parser(fen: &str) -> Result<ChessPosition, String> {
    let mut pieces = [ChessPiece::None; 64];
    let mut index: usize = 0;
    let split_fen: Vec<&str> = fen.split(' ').collect();
    let mut white_castling = Castling {
        queen_side: false,
        king_side: false,
    };
    let mut black_castling = Castling {
        queen_side: false,
        king_side: false,
    };
    for character in split_fen[0].chars() {
        if split_fen[2].contains('Q') {
            white_castling.queen_side = true;
        }
        if split_fen[2].contains('K') {
            white_castling.king_side = true;
        }
        if split_fen[2].contains('q') {
            black_castling.queen_side = true;
        }
        if split_fen[2].contains('k') {
            black_castling.king_side = true;
        }
        let color = match character.is_uppercase() {
            true => ChessColor::White,
            false => ChessColor::Black,
        };
        match character {
            'p' | 'P' => pieces[index] = ChessPiece::Pawn(color),
            'n' | 'N' => pieces[index] = ChessPiece::Knight(color),
            'b' | 'B' => pieces[index] = ChessPiece::Bishop(color),
            'r' | 'R' => pieces[index] = ChessPiece::Rook(color, RookType::Unknown),
            'q' | 'Q' => pieces[index] = ChessPiece::Queen(color),
            'k' | 'K' => pieces[index] = ChessPiece::King(color),
            '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' => {
                index += character.to_string().parse::<i32>().unwrap() as usize;
                continue;
            }
            _ => {
                continue;
            }
        }

        index += 1;
        if index >= 64 {
            break;
        }
    }
    let turn = match split_fen[1] {
        "w" => ChessColor::White,
        "b" => ChessColor::Black,
        _ => return Err("Invaild fen, incorrect turn?".into()),
    };

    if white_castling.queen_side {
        pieces[(QUEEN_SIDE_ROOK_X + BOARD_SIZE * 7) as usize] =
            ChessPiece::Rook(ChessColor::White, RookType::QueenSide);
    }
    if white_castling.king_side {
        pieces[(KING_SIDE_ROOK_X + BOARD_SIZE * 7) as usize] =
            ChessPiece::Rook(ChessColor::White, RookType::KingSide);
    }
    if black_castling.queen_side {
        pieces[QUEEN_SIDE_ROOK_X as usize] =
            ChessPiece::Rook(ChessColor::Black, RookType::QueenSide);
    }
    if black_castling.queen_side {
        pieces[KING_SIDE_ROOK_X as usize] = ChessPiece::Rook(ChessColor::Black, RookType::KingSide);
    }
    Ok(ChessPosition {
        pieces,
        turn,
        castle_data: [white_castling, black_castling],
        en_passant: None,
    })
}

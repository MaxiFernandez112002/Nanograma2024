/*import React, { useEffect, useState } from 'react';
import PengineClient from './PengineClient';
import Board from './Board';

let pengine;

function Game() {

  // State
  const [grid, setGrid] = useState(null);
  const [rowsClues, setRowsClues] = useState(null);
  const [colsClues, setColsClues] = useState(null);
  const [waiting, setWaiting] = useState(false);

  useEffect(() => {
    // Creation of the pengine server instance.    
    // This is executed just once, after the first render.    
    // The callback will run when the server is ready, and it stores the pengine instance in the pengine variable. 
    PengineClient.init(handleServerReady);
  }, []);

  function handleServerReady(instance) {
    pengine = instance;
    const queryS = 'init(RowClues, ColumClues, Grid)';
    pengine.query(queryS, (success, response) => {
      if (success) {
        setGrid(response['Grid']);
        setRowsClues(response['RowClues']);
        setColsClues(response['ColumClues']);
      }
    });
  }
  
  function handleClick(i, j) {
    // No action on click if we are waiting.
    if (waiting) {
      return;
    }
    // Build Prolog query to make a move and get the new satisfacion status of the relevant clues.    
    const squaresS = JSON.stringify(grid).replaceAll('"_"', '_'); // Remove quotes for variables. squares = [["X",_,_,_,_],["X",_,"X",_,_],["X",_,_,_,_],["#","#","#",_,_],[_,_,"#","#","#"]]
    const content = '#'; // Content to put in the clicked square.
    const rowsCluesS = JSON.stringify(rowsClues);
    const colsCluesS = JSON.stringify(colsClues);
    const queryS = `put("${content}", [${i},${j}], ${rowsCluesS}, ${colsCluesS}, ${squaresS}, ResGrid, RowSat, ColSat)`; // queryS = put("#",[0,1],[], [],[["X",_,_,_,_],["X",_,"X",_,_],["X",_,_,_,_],["#","#","#",_,_],[_,_,"#","#","#"]], GrillaRes, FilaSat, ColSat)
    setWaiting(true);
    pengine.query(queryS, (success, response) => {
      if (success) {
        setGrid(response['ResGrid']);
      }
      setWaiting(false);
    });
  }

  if (!grid) {
    return null;
  }

  const statusText = 'Keep playing!';
  return (
    <div className="game">
      <Board
        grid={grid}
        rowsClues={rowsClues}
        colsClues={colsClues}
        onClick={(i, j) => handleClick(i, j)}
      />
      <div className="game-info">
        {statusText}
      </div>
    </div>
  );
}

export default Game;*/

/*

import React, { useEffect, useState } from 'react';
import PengineClient from './PengineClient';
import Board from './Board';

let pengine;

function Game() {
    // State
    const [grid, setGrid] = useState(null);
    const [rowsClues, setRowsClues] = useState(null);
    const [colsClues, setColsClues] = useState(null);
    const [waiting, setWaiting] = useState(false);
    const [selectedContent, setSelectedContent] = useState('#'); // seteamos el contenido por default en '#'

    useEffect(() => {
        // Creation of the pengine server instance.
        // This is executed just once, after the first render.
        // The callback will run when the server is ready, and it stores the pengine instance in the pengine variable.
        PengineClient.init(handleServerReady);
    }, []);

    function handleServerReady(instance) {
        pengine = instance;
        const queryS = 'init(RowClues, ColumClues, Grid)';
        pengine.query(queryS, (success, response) => {
            if (success) {
                setGrid(response['Grid']);
                setRowsClues(response['RowClues']);
                setColsClues(response['ColumClues']);
            }
        });
    }

    function handleClick(i, j) {
        // No action on click if we are waiting.
        if (waiting) {
            return;
        }
        // Build Prolog query to make a move and get the new satisfaction status of the relevant clues.
        const squaresS = JSON.stringify(grid).replaceAll('"_"', '_'); // Remove quotes for variables.
        const content = selectedContent; // Content to put in the clicked square.
        const rowsCluesS = JSON.stringify(rowsClues);
        const colsCluesS = JSON.stringify(colsClues);
        const queryS = `put("${content}", [${i},${j}], ${rowsCluesS}, ${colsCluesS}, ${squaresS}, ResGrid, RowSat, ColSat)`;

        // Set waiting state to true while waiting for response from server.
        setWaiting(true);
        // Send query to server using pengine instance.
        pengine.query(queryS, (success, response) => {
            if (success) {
                // Update the grid with the new content.
                setGrid(response['ResGrid']);
            }
            // Set waiting state back to false after receiving response.
            setWaiting(false);
        });
    }

    // Function to handle clicking on the 'X' button
    function handleSelectX() {
        setSelectedContent('X'); // Set selected content to 'X'
    }

    // Function to handle clicking on the '#' button
    function handleSelectHash() {
        setSelectedContent('#'); // Set selected content to '#'
    }

    if (!grid) {
        return null;
    }

    const statusText = 'Keep playing!';
    return (
        <div className="game">
            <div>
                <button onClick={handleSelectX}>Select X</button>
                <button onClick={handleSelectHash}>Select #</button>
            </div>
            <Board
                grid={grid}
                rowsClues={rowsClues}
                colsClues={colsClues}
                onClick={(i, j) => handleClick(i, j)}
            />
            <div className="game-info">
                {statusText}
            </div>
        </div>
    );
}

export default Game;*/
import React, { useEffect, useState } from 'react';
import PengineClient from './PengineClient';
import Board from './Board';

let pengine;

function Game() {
    // State
    const [grid, setGrid] = useState(null);
    const [rowsClues, setRowsClues] = useState(null);
    const [colsClues, setColsClues] = useState(null);
    const [waiting, setWaiting] = useState(false);
    const [selectedContent, setSelectedContent] = useState('#'); // seteamos el contenido por default en '#'

    useEffect(() => {
        // Creation of the pengine server instance.
        // This is executed just once, after the first render.
        // The callback will run when the server is ready, and it stores the pengine instance in the pengine variable.
        PengineClient.init(handleServerReady);
    }, []);

    function handleServerReady(instance) {
        pengine = instance;
        const queryS = 'init(RowClues, ColumClues, Grid)';
        pengine.query(queryS, (success, response) => {
            if (success) {
                setGrid(response['Grid']);
                setRowsClues(response['RowClues']);
                setColsClues(response['ColumClues']);
            }
        });
    }

    function handleClick(i, j) {
        // No action on click if we are waiting.
        if (waiting) {
            return;
        }
        // Build Prolog query to make a move and get the new satisfaction status of the relevant clues.
        const squaresS = JSON.stringify(grid).replaceAll('"_"', '_'); // Remove quotes for variables.
        const content = selectedContent; // Content to put in the clicked square.
        const rowsCluesS = JSON.stringify(rowsClues);
        const colsCluesS = JSON.stringify(colsClues);
        const queryS = `put("${content}", [${i},${j}], ${rowsCluesS}, ${colsCluesS}, ${squaresS}, ResGrid, RowSat, ColSat)`;

        // Set waiting state to true while waiting for response from server.
        setWaiting(true);
        // Send query to server using pengine instance.
        pengine.query(queryS, (success, response) => {
            if (success) {
                // Update the grid with the new content.
                setGrid(response['ResGrid']);
            }
            // Set waiting state back to false after receiving response.
            setWaiting(false);
        });
    }

    // Function to handle clicking on the 'X' button
    function handleSelectX() {
        setSelectedContent('X'); // Set selected content to 'X'
    }

    // Function to handle clicking on the '#' button
    function handleSelectHash() {
        setSelectedContent('#'); // Set selected content to '#'
    }

    function handleComprobar() {
      if (!grid || !rowsClues || !colsClues) {
          return; // No se puede comprobar si no se han inicializado la grilla y las pistas.
      }
      
      // Convertir la grilla y las pistas a cadenas JSON para enviarlas como argumentos a la consulta Prolog.
      const gridS = JSON.stringify(grid);
      const rowsCluesS = JSON.stringify(rowsClues);
      const colsCluesS = JSON.stringify(colsClues);
  
      // Construir la consulta Prolog con los argumentos necesarios.
      const queryS = `comprobar_grilla(${gridS}, ${rowsCluesS}, ${colsCluesS}, FilaSat, ColSat)`;
  
      // Enviar la consulta al servidor Pengine.
      pengine.query(queryS, (success, response) => {
          if (success) {
              // Handle la respuesta del servidor aquí.
              console.log('Felicidades ganaste!:', response);
              alert('¡Felicidades, ganaste!');
          } else {
              console.error('Error al ejecutar la consulta Prolog.');
          }
          
      });
  }
  

    if (!grid) {
        return null;
    }

    const statusText = 'Keep playing!';
    return (
        <div className="game">
            <div>
                <button onClick={handleSelectX}>Select X</button>
                <button onClick={handleSelectHash}>Select #</button>
            </div>
            <Board
                grid={grid}
                rowsClues={rowsClues}
                colsClues={colsClues}
                onClick={(i, j) => handleClick(i, j)}
            />
            <div className="game-buttons">
                <button onClick={handleComprobar}>Comprobar</button>
            </div>
            <div className="game-info">
                {statusText}
            </div>
        </div>
    );
}

export default Game;



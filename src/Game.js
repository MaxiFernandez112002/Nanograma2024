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
    const [filasSatisfechas, setFilasSatisfechas] = useState([]);
    const [columnasSatisfechas, setColumnasSatisfechas] = useState([]);
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
                setFilasSatisfechas(Array(response['RowClues'].length).fill(0));
                setColumnasSatisfechas(Array(response['ColumClues'].length).fill(0));
                //FALTA CHEQUEAR SI HAY PISTAS QUE SE CUMPLEN AL INICIO, HAY QUE LLAMAR AL QUERY VERIFICARFILA Y VERIFICAR COLUMNA PARA QUE MIRE SI ESTA SATISFECHA ALGUNAN DE ESTAS AL INICIO
            }
        });
    }

    function handleClick(i, j, content) {
        // No action on click if we are waiting.
        if (waiting) {
            return;
        }
        // Build Prolog query to make a move and get the new satisfaction status of the relevant clues.
        const squaresS = JSON.stringify(grid).replaceAll('"_"', '_'); // Remove quotes for variables.
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

                if (response['RowSat'] === 1)
                    filasSatisfechas[i] = 1;
                else
                    filasSatisfechas[i] = 0;

                if (response['ColSat'] === 1)
                    columnasSatisfechas[j] = 1;
                else
                    columnasSatisfechas[j] = 0;
            }
            // Set waiting state back to false after receiving response.
            setWaiting(false);
        });
    }

    function handleToggleContent() {
        setSelectedContent(selectedContent === 'X' ? '#' : 'X');
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
              alert('!ESTA GODOYCRUZ ESTO!');
          } else {
              console.error('Error al ejecutar la consulta Prolog.');
              alert('tu solucion no es GOD, fijate lo que haces');
          }
          
      });
  }
  

    if (!grid) {
        return null;
    }

    const statusText = 'MODO CLASICO';
    return (
        <div>
                <Board
                    grid={grid}
                    rowsClues={rowsClues}
                    colsClues={colsClues}
                    onClick={(i, j) => handleClick(i, j, selectedContent)}
                    filasSatisfechas={filasSatisfechas}
                    columnasSatisfechas={columnasSatisfechas}
                />
            <div>
                <button className='boton-modo'
                 onClick={handleToggleContent}>Cambiar modo</button>
                <button className='boton-comprobar'
                 onClick={handleComprobar}>Comprobar</button>
            <div>
            <div className="game-info">
                {statusText}
            </div>
        </div>
        </div>
        </div>
    );
}

export default Game;




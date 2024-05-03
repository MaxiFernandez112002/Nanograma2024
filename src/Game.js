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
    const [initialState, setInitialState] = useState(null);

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
                 // Inicializar las filas y columnas satisfechas

                /*%%%%%%%%%%%%%%%%%%%%%%%%%%*/ 
                 setInitialState(response); // Guarda el estado inicial
                /*%%%%%%%%%%%%%%%%%%%%%%%%%%*/ 

                const rowsCluesS = JSON.stringify(response['RowClues']);
                const colsCluesS = JSON.stringify(response['ColumClues']);
                const squaresS = JSON.stringify(response['Grid']).replaceAll('"_"', '_');


                for (let i = 0; i < response['RowClues'].length; i++) {
                    const FilaSat = `verificar_fila(${i}, ${rowsCluesS}, ${squaresS}, FilaSat)`;
                    pengine.query(FilaSat, (success, response) => {   
                        if (success) {
                            // Update the grid with the new content.
                            if (response['FilaSat'] === 1)
                                filasSatisfechas[i] = 1;
                            else
                                filasSatisfechas[i] = 0;    
                        } 
                    });
                }

                for (let j = 0; j < response['ColumClues'].length; j++) {
                    const ColSat = `verificar_columna(${j}, ${colsCluesS}, ${squaresS}, ColSat)`;
                    pengine.query(ColSat, (success, response) => {
                        if (success){      
                            if (response['ColSat'] === 1)
                                columnasSatisfechas[j] = 1;
                            else
                                columnasSatisfechas[j] = 0;   
                        }
                    });
                }

                setFilasSatisfechas(filasSatisfechas);
                setColumnasSatisfechas(columnasSatisfechas);

                // Construir la consulta Prolog con los argumentos necesarios.
                 const queryP = `comprobar_grilla(${squaresS}, ${rowsCluesS}, ${colsCluesS}, FilaSat, ColSat)`;
  
                // Enviar la consulta al servidor Pengine.
                pengine.query(queryP, (successp, responsep) => {
                if (successp) {
                    // Handle la respuesta del servidor aquí.
                    console.log('Felicidades ganaste!:', responsep);
                    alert('!GANASTE, FELICIDADEZ!');
                } else {
                     console.error('Error al ejecutar la consulta Prolog.');
            }
          
      });

                
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
              alert('!GANASTE, FELICIDADES!');
          } else {
              console.error('Error al ejecutar la consulta Prolog.');
          }
          
      });
  }
  

    if (!grid) {
        return null;
    }

    const statusText = 'MODO CLASICO';
    /*
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
                 onClick={handleToggleContent}>Cambiar a modo {selectedContent === 'X' ? '#' : 'X'}</button>
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
}*/
function handleRestart() {
    if (initialState) {
        setGrid(initialState['Grid']);
        setRowsClues(initialState['RowClues']);
        setColsClues(initialState['ColumClues']);
        setFilasSatisfechas(Array(initialState['RowClues'].length).fill(0));
        setColumnasSatisfechas(Array(initialState['ColumClues'].length).fill(0));
    }
}


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
            <button className='boton-modo' onClick={handleToggleContent}>Cambiar a modo {selectedContent === 'X' ? '#' : 'X'}</button>
            <button className='boton-comprobar' onClick={handleComprobar}>Comprobar</button>
            <button className='boton-reiniciar' onClick={handleRestart}>Reiniciar Juego</button>
        </div>
        <div className="game-info">
            {statusText}
        </div>
    </div>
);
}

export default Game;




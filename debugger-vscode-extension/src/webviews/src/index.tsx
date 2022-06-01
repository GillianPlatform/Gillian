import React from 'react'
import ReactDOM from 'react-dom'
import App from './App'
import './vscode.css'
import { VSCodeAPI } from './VSCodeAPI'


VSCodeAPI.postMessage({
  type: 'refreshFiles',
})

VSCodeAPI.postMessage({
  type: 'refreshState',
})

window.addEventListener('message', e => {
  // @ts-ignore
  const message = e.data
  if (message.command === 'updateState') {
  }
})

ReactDOM.render(
  <React.StrictMode>
    <App />
  </React.StrictMode>,
  document.getElementById('root')
)

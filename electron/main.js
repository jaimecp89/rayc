const {
  app, BrowserWindow, protocol
} = require('electron');

const fs = require('fs')
const path = require('path')

let mainWindow

function createApp () {
  protocol.interceptStringProtocol('http', function (req, callback) {
    const filePath = path.join('.', req.url.split('http://-/')[1])
    const data = fs.readFileSync(filePath, 'utf8')

    callback({
      mimeType: req.headers['Accept'].split(',')[0],
      data
    });
  }, (error) => {
    if (error) {
      throw Error('failed to register protocol handler for HTTP')
    }

    createWindow();
  })
}

function createWindow() {
  mainWindow = new BrowserWindow({
    width: 800,
    height: 600,
    webPreferences: {
      nodeIntegration: true,
      contextIsolation: false
    },
    autoHideMenuBar: true
  })
  mainWindow.loadURL('http://-/index.html')

  mainWindow.on('closed', function() {
    mainWindow = null
  })
}

app.on('ready', createApp)

app.on('window-all-closed', function() {
  if (process.platform !== 'darwin') {
    app.quit()
  }
})

app.on('activate', function() {
  if (mainWindow === null) {
    createWindow()
  }
})

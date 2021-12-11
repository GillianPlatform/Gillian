function new_section {
    param (
        $SectionName
    )
    echo ""
    echo ""
    echo "========"
    echo $SectionName
}


function Test-CommandExists {
 param ($command)
 try {if(Get-Command $command){ return $true; }}
 Catch { return $false; }
}

$env:REGISTRY_URL = "http://localhost:4873/"

if (!(Test-CommandExists "verdaccio")) {
    new_section "Installing verdaccio"
    yarn global add verdaccio
}

function IsPortOccupied ($portNo) {
   try {if( Get-Process -Id (Get-NetTCPConnection -LocalPort $portNo).OwningProcess) { return $true; }}
   Catch { return $false; }
}

function rimraf ($path) {
  try { rm -Recurse -Force $path -erroraction stop }
  catch [System.Management.Automation.ItemNotFoundException] { $null }
}

if (!(IsPortOccupied 4873 )) {
    new_section "Setting up verdaccio"
    New-Item -ItemType Directory -Force -Path ~/.config/verdaccio,~/.local/share/verdaccio/storage
    cp -Force ./.ci/verdaccio-config.yaml ~\.config\verdaccio\config.yaml
    Start-Job { verdaccio }
    Start-Sleep -s 1
}

new_section "Packaging for NPM"
node scripts/package.js 
new_section "Publishing to local NPM"
npm publish --registry $env:REGISTRY_URL ./package.tar.gz

cd esy-test/
$env:ESY__PREFIX = "$env:HOME/_esy_test/prefix"
rimraf $env:ESY__PREFIX
New-Item -ItemType Directory -Force -Path $env:ESY__PREFIX
esy i --npm-registry $env:REGISTRY_URL
esy b

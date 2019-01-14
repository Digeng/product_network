



function getTabIdByName(tabName) {
	let tabset = document.getElementById('tabset')
	let lis = tabset.getElementsByTagName('li')

	let tabNames = Array.from(lis).map(li => {
		let title = li.innerText 
		title = title.match("[a-zA-Z -_]*")[0]  //strips out leading '#' character of tabname 
		return title
	})

	let tabIdx = -1
	for (let i = 0; i < tabNames.length; ++i) {
		if (tabNames[i] === tabName) {
			tabIdx = i
			break
		}
	}

	let tab = lis[tabIdx]
	let tabId = tab.getElementsByTagName("a")[0].href.match("tab.*")[0]
	return tabId
}


function createBlockTab() {
	const blockId = getTabIdByName("Block")

	let tabPane = document.getElementById(blockId)
	const tabPaneScore = "<div id='score'> 0 </div>"
	const tabPaneCanvas = "<canvas id='canvas' height='400' width='240'/>"
	tabPane.innerHTML = tabPane.innerHTML + tabPaneScore + tabPaneCanvas
}


/*function resizeNetworkCanvas(width, height) {
	let canvas = document.querySelector(".vis-network canvas")
	canvas.setAttribute("width", width)
	canvas.setAttribute("height", height)
}*/


createBlockTab()


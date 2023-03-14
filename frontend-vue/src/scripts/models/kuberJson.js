export class KuberJson {
	constructor() {
		this.selections = []
		this.inputs = []
		this.referenceInputs = []
		this.outputs = []
		this.collaterals = "" 
	}

	_pushSelection(selection) {
		this.selections.push(selection)
	}

	_pushInput(input) {
		this.inputs.push(input)
	}
	
	_pushReferenceInput(referenceInput) {
		this.referenceInputs.push(referenceInput)
	}

	_pushOutput(output) {
		this.outputs.push(output)
	}

	addSelectionTx(txHash, index) {
		var selection = txHash + "#" + index 
		this._pushSelection(selection)
	}

	addSelectionUtxoCborHex(cborHex) {
		this._pushSelection(cborHex)
	}

	addSelectionAddrBech32(addrBech32) {
		this._pushSelection(addrBech32)
	}

	addSelectionAddrCborHex(addrCborHex) {
		this._pushSelection(addrCborHex)
	}

	addOutput(address, value, datum=undefined) {
		var output = {
			"address": address,
			"value": value,
		}
		if (datum != undefined) {
			output["datum"] = datum
		}
		this._pushOutput(output)
	}

	addInputPubKeyUtxos(txHash, index) {
		var input = txHash + "#" + index 
		this._pushInput(input)
	}

	addInputsScript(utxoHash, utxoIndex, scriptType, scriptDescription, scriptCbor, redeemer={"constructor":0,"fields":[]}, datum=undefined) {
		var input = {
			"utxo": utxoHash + "#" + utxoIndex,
			"script": {
				"type": scriptType,
				"description": scriptDescription,
				"cborHex": scriptCbor
			},
			"redeemer": redeemer
		}
		if (datum != undefined) {
			input["datum"] = datum
		}
		this._pushInput(input)
	}

	addCollateralTx(txHash, index) {
		var collateral = txHash + "#" + index 
		this.collaterals = collateral
	}

	addCollateralAddrBech32(addrBech32) {
		this.collaterals = addrBech32
	}

	addCollateralCborHex(addrCborHex) {
		this.collaterals = addrCborHex
	}

	getJsonString(){
		var res = {
			"selections": this.selections,
			"inputs": this.inputs,
			"outputs": this.outputs
		}
		if (this.collaterals != "") {
			res["collaterals"] = this.collaterals
		}
		if (this.referenceInputs.length) {
			res["referenceInputs"] = this.referenceInputs
		}
		return JSON.stringify(res)
	}
}

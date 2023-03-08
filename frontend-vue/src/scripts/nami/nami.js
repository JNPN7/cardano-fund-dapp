import * as S from "@emurgo/cardano-serialization-lib-asmjs"
import {Buffer} from "buffer";

export class NamiWalletApi {
	constructor(nami) {
        this.Nami = nami;
    }

	async isInstalled() {
		if (this.Nami) return true;
		else return false;
	}

	async isEnabled() {
		return await this.Nami.isEnabled();
	}

	async enable() {
		if (!await this.isEnabled()) {
		  try {
			return await this.Nami.enable();
		  } catch (error) {
			throw error;
		  }
		}
	}

	async getHexAddress(){
		const addressHex = Buffer.from(
			(await this.Nami.getUsedAddresses())[0],
			"hex"
		);
	  	return addressHex
	}

	async getAddress() {
		if (!this.isEnabled()) throw ERROR.NOT_CONNECTED;
		const addressHex = await this.getHexAddress() 
		const address = S.BaseAddress.from_address(
			S.Address.from_bytes(addressHex)
		)
		.to_address()
		.to_bech32();

		return address;
	}

	async getNetworkId() {
    	if (!this.isEnabled()) throw ERROR.NOT_CONNECTED;
		let networkId = await this.Nami.getNetworkId()
		return {
		  id: networkId,
		  network: networkId === 1 ? 'mainnet' : 'testnet'
		}
	}

	async getBalance() {
    	if (!this.isEnabled()) throw ERROR.NOT_CONNECTED;
		let networkId = await this.getNetworkId();
		//let protocolParameter = await this._getProtocolParameter(networkId.id)

		const valueCBOR = await this.Nami.getBalance()
		console.log(valueCBOR)
		const value = S.Value.from_bytes(Buffer.from(valueCBOR, "hex"))
		console.log(value)

		const utxos = await this.Nami.getUtxos()
		console.log("utxos", utxos)
		const parsedUtxos = utxos.map((utxo) => S.TransactionUnspentOutput.from_bytes(Buffer.from(utxo, "hex")))
		console.log("parsed utxos", parsedUtxos)
	}
}

export async function test(){
	console.log("here")
}
export default NamiWalletApi;

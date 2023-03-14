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

	async getPaymentPubKeyHash() {
		var address = await this.getHexAddress()
		const paymentKeyHash = S.BaseAddress.from_address(
            S.Address.from_bytes(
                Buffer.from(address, "hex")

            ))
            .payment_cred()
            .to_keyhash();
		console.log(paymentKeyHash.to_hex())
	}

	async getHexAddress() {
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

export async function signAndSubmit(provider, _tx) {
    let tx;
    try {
        const txArray=Uint8Array.from(Buffer.from(_tx, 'hex'))
      tx = S.Transaction.from_bytes(txArray)
      if (tx.auxiliary_data()) {
        const _txBody = tx.body()
        const txBody = S.TransactionBody.new(_txBody.inputs(),_txBody.outputs(),_txBody.fee(),_txBody.ttl())
        txBody.set_auxiliary_data_hash(hash_auxiliary_data(tx.auxiliary_data()))
        if(_txBody.collateral())
            txBody.set_collateral(_txBody.collateral())
        if(_txBody.mint())
            txBody.set_mint(_txBody.mint())
        if(_txBody.required_signers()){
            txBody.set_required_signers(_txBody.required_signers())
        }
        if(_txBody.validity_start_interval_bignum()) {
            txBody.set_validity_start_interval_bignum(_txBody.validity_start_interval_bignum())
        }
        if(_txBody.network_id()){
            txBody.set_network_id(_txBody.network_id())
        }
        tx = S.Transaction.new(txBody, tx.witness_set(), tx.auxiliary_data())
      }
    } catch (e) {
      throw new Error("Invalid transaction string :"+ e.message)
    }

    const witnesesRaw = await provider.signTx(
        Buffer.from(tx.to_bytes()).toString('hex'),
        true
    )
    const newWitnesses = S.TransactionWitnessSet.from_bytes(Buffer.from(witnesesRaw, "hex"))
    const newWitnessSet = S.TransactionWitnessSet.new();
    if (tx.witness_set().plutus_data())
        newWitnessSet.set_plutus_data(tx.witness_set().plutus_data());
    if (tx.witness_set().plutus_scripts())
        newWitnessSet.set_plutus_scripts(tx.witness_set().plutus_scripts())
    if (tx.witness_set().redeemers())
        newWitnessSet.set_redeemers(tx.witness_set().redeemers())
    if (tx.witness_set().native_scripts())
        newWitnessSet.set_native_scripts(tx.witness_set().native_scripts())
    // add the new witness.
    if (tx.witness_set().vkeys()) {
        const newVkeySet = S.Vkeywitnesses.new()

        for (let i = 0; i < tx.witness_set().vkeys().len(); i++) {
            newVkeySet.add(tx.witness_set().vkeys().get(i))
        }
        for (let i = 0; i < newWitnesses.vkeys().len(); i++) {
            newVkeySet.add(newWitnesses.vkeys().get(i))
        }
        newWitnessSet.set_vkeys(newVkeySet)

    } else {
        newWitnessSet.set_vkeys(newWitnesses.vkeys())
    }
    tx = S.Transaction.new(tx.body(), newWitnessSet, tx.auxiliary_data());
    const signedTxString = Buffer.from(tx.to_bytes()).toString('hex')
    // @ts-ignore
    console.log({
        additionWitnessSet: witnesesRaw,
        finalTx: signedTxString
    })
    return provider.submitTx(signedTxString)
}

export async function test(){
	console.log("here")
}
export default NamiWalletApi;

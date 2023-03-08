import * as S from "../cardano-serialization-lib-asmjs/cardano_serialization_lib.js"

class NamiWalletApi {
	constructor(nami, apiKey) {
        this.apiKey  = apiKey;
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

	async getAddress() {
		if (!this.isEnabled()) throw ERROR.NOT_CONNECTED;
		const addressHex = Buffer.from(
			(await this.Nami.getUsedAddresses())[0],
			"hex"
		);
		const address = S.BaseAddress.from_address(
			S.Address.from_bytes(addressHex)
		)
		.to_address()
		.to_bech32();

		return address;
	}
}

export default NamiWalletApi;

var nami = new NamiWalletApi(window.cardano, "some api");

var connect_btn = document.getElementById("connect");
if (nami.isEnabled()) {
	connect_btn.classList.add("d-none");
}

console.log(await window.cardano.getUsedAddresses())
console.log(nami.getAddress())
connect_btn.onclick = nami.enable();


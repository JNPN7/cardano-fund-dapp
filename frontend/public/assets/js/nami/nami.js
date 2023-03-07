

class NamiWalletApi {
	constructor(nami, apiKey, serializableLib) {
        this.apiKey  = apiKey;
        this.Nami = nami;
		this.S = serializableLib;
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
		const address = this.S.BaseAddress.from_address(
			this.S.Address.from_bytes(addressHex)
		)
		.to_address()
		.to_bech32();

		return address;
	}
}

nami = new NamiWalletApi(window.cardano, "some api");

connect_btn = document.getElementById("connect");
if (nami.isEnabled()) {
	connect_btn.classList.add("d-none");
}

connect_btn.onclick = nami.enable();


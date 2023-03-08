<script setup>
import Button from "../components/Button.vue"
</script>

<template>
	<div class="container">
		<div class="glass p-4 center-window">
			<div class="col">
				<div class="form">
					<div class="mb-5">
						<div class="h2 text-center"> Let's Fund </div>
					</div>
					<div class="mb-4">
						<input id="money" class="form-control" name="money" placeholder="Ada">
					</div>
					<div class="text-center">
						<button class="btn btn-primary"> Fund </button>
					</div>
			</div>
			</div>
		</div>
	</div>
</template>

<script>
export default {
	name: "Fund",
	Components: {
		Button
	}
}
</script>

<style scoped>
</style>

open Shared

let txt2img_query =
  Yojson_safe.from_string
    {|
{
  "enable_hr": false,
  "denoising_strength": 0.7,
  "firstphase_width": 0,
  "firstphase_height": 0,
  "hr_scale": 2,
  "hr_upscaler": "Latent",
  "hr_prompt": "",
  "prompt": "",
  "styles": [ ],
  "seed": -1,
  "subseed": -1,
  "subseed_strength": 0,
  "seed_resize_from_h": -1,
  "seed_resize_from_w": -1,
  "sampler_name": "Euler a",
  "batch_size": 1,
  "n_iter": 1,
  "steps": 50,
  "cfg_scale": 7,
  "width": 512,
  "height": 512,
  "restore_faces": false,
  "tiling": false,
  "do_not_save_samples": false,
  "do_not_save_grid": false,
  "negative_prompt": "string",
  "s_min_uncond": 0,
  "s_churn": 0,
  "s_tmax": 0,
  "s_tmin": 0,
  "s_noise": 1,
  "override_settings": {"show_progress_every_n_steps": 5, "show_progress_type": "Full"},
  "override_settings_restore_afterwards": true,
  "script_args": [],
  "sampler_index": "Euler a",
  "script_name": null,
  "send_images": true,
  "save_images": true,
  "alwayson_scripts": {}
}
|}
;;

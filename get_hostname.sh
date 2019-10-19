get_model() {
if [[ -d "/system/app/" && -d "/system/priv-app" ]]; then
                model="$(getprop ro.product.brand) $(getprop ro.product.model)"

            elif [[ -f "/sys/devices/virtual/dmi/id/product_name" ||
                    -f "/sys/devices/virtual/dmi/id/product_version" ]]; then
                model="$(< /sys/devices/virtual/dmi/id/product_name)"
                # model+=" $(< /sys/devices/virtual/dmi/id/product_version)"

            elif [[ -f "/sys/firmware/devicetree/base/model" ]]; then
                model="$(< /sys/firmware/devicetree/base/model)"

            elif [[ -f "/tmp/sysinfo/model" ]]; then
                model="$(< /tmp/sysinfo/model)"
fi
}
get_model                       # get the laptop model
echo $model

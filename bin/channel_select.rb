#!/usr/bin/env ruby
# encoding=utf-8

# 指定されたファイルのヒストグラムを
# 三つのハッシュ(RGBの順)で返す。
def histogram(path)
  output = IO.popen([
      'convert', path,
      '-channel', 'RGB',
      '-separate',
      '-format', '%c',
      'histogram:info:-']) do |io|
    io.read
  end

  output.split(/^$/, 3).map do |txt|
    hist = {}
    txt.scan(/^ +(\d+): \( *(\d+),/) do
      hist[$2.to_i] = $1.to_i
    end
    hist
  end
end

# ヒストグラムを黒側、白側の二つに分けて
# それぞれのピクセル数(:pixel)、
# ピクセルごとの明度の平均値(:mean)、
# 同じく中央値(:median)、分散(:variance)、
# 四分位数範囲(:iqr)を返す。
def bw_statistics(histogram, th = 128)
  black = {pixel: 0, mean: 0.0, variance: 0.0}
  white = {pixel: 0, mean: 0.0, variance: 0.0}

  histogram.each do |value, pixel|
    if value < th
      h = black
    else
      h = white
    end
    h[:pixel] += pixel
    h[:mean] += pixel*value
  end

  [black, white].each do |h|
    h[:mean] /= h[:pixel].to_f
    h[:quartile1] = -h[:pixel]/4
    h[:quartile2] = -h[:pixel]/2
    h[:quartile3] = -h[:pixel]*3/4
  end

  histogram.sort.each do |value, pixel|
    if value < th
      h = black
    else
      h = white
    end

    h[:variance] += (h[:mean] - value)**2*pixel
    [:quartile1, :quartile2, :quartile3].each do |sym|
      if h[sym] < 0
        h[sym] += pixel
        h[sym] = value if h[sym] >= 0
      end
    end
  end

  [black, white].each do |h|
    h[:variance] /= h[:pixel].to_f
    h[:median] = h[:quartile2]
    h[:iqr] = h[:quartile3] - h[:quartile1]
  end

  [black, white]
end

# 後処理にもっとも都合のよいと思われる
# チャネル名とその情報を返す。
#
# 判定方法をブロックで与えることができる。
def best_channel(path, &sort_index)
  sort_index ||= proc do |channel, (black, white)|
    [white[:iqr], black[:iqr], white[:variance], black[:variance]]
  end

  [:red, :green, :blue].zip(histogram(path)).
    map {|c, h| [c, bw_statistics(h)] }.
    sort_by(&sort_index).
    first
end

p best_channel(ARGV.shift)
